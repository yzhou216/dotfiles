#!/usr/bin/env -S cargo +nightly -Zscript
---
[package]
edition = "2024"
[dependencies]
anyhow = "1"
regex = "1"
url = "2"
reqwest = { version = "0.12", default-features = false, features = ["blocking", "cookies", "rustls-tls"] }
---
//! Accepts the USFConnect AUP on the Cisco ISE captive portal.
//!
//! Usage: usfconnect.rs [portal-url]   (the URL is only needed if detection fails)
use anyhow::{Context, Result, bail};
use regex::Regex;
use reqwest::blocking::{Client, Response};
use reqwest::header::LOCATION;
use reqwest::{StatusCode, redirect};
use std::time::Duration;
use url::Url;

const UA: &str = "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0";

/// The ISE nodes rotate (guestaccess1..N.usfca.edu), so never pin one host.
const PORTAL_HOST: &str = r"guestaccess\d*\.usfca\.edu";

/// ISE answers its ajax endpoints with {"status":..,"messages":[..]}.
const STATUS_OK: &str = r#""status":"success""#;
const STATUS_ERR: &str = r#""status":"error""#;

fn re(pattern: &str) -> Regex {
    Regex::new(pattern).expect("valid regex")
}

/// First capture group of `pattern`.
fn cap(pattern: &str, text: &str) -> Option<String> {
    re(pattern).captures(text).map(|c| c[1].to_string())
}

/// The `<input>`/`<form>` tag carrying `name="<name>"`.
fn tag<'a>(html: &'a str, kind: &str, name: &str) -> Option<&'a str> {
    re(&format!(r#"<{kind}\b[^>]*name="{}"[^>]*>"#, regex::escape(name)))
        .find(html)
        .map(|m| m.as_str())
}

fn attr(tag: &str, name: &str) -> Option<String> {
    cap(&format!(r#"\s{}="([^"]*)""#, regex::escape(name)), tag)
}

/// One `name: 'value'` entry from the page's own `page.init({...})` call.
fn js_param(html: &str, name: &str) -> Option<String> {
    cap(&format!(r"\b{}\s*:\s*'([^']*)'", regex::escape(name)), html)
        .filter(|value| !value.is_empty())
}

/// The headline ISE prints on a page, e.g. the notice shown once accepted.
fn instruction_message(html: &str) -> Option<String> {
    let block = cap(r"(?s)cisco-ise-instruction-message[^>]*>(.*?)</p>", html)?;
    let text = re("<[^>]*>").replace_all(&block, " ");
    Some(text.split_whitespace().collect::<Vec<_>>().join(" ")).filter(|t| !t.is_empty())
}

fn json_has(body: &str, needle: &str) -> bool {
    let compact: String = body.chars().filter(|c| !c.is_whitespace()).collect();
    compact.contains(needle)
}

fn client() -> reqwest::blocking::ClientBuilder {
    Client::builder()
        .user_agent(UA)
        // Whatever intercepts the connection answers with its own certificate.
        .danger_accept_invalid_certs(true)
        .timeout(Duration::from_secs(10))
}

struct Portal {
    client: Client,
    /// The page's own URL, so its relative form actions resolve against it.
    base: Url,
    token: String,
    sid: String,
    html: String,
}

impl Portal {
    fn open(url: &str) -> Result<Self> {
        let client = client()
            .cookie_store(true)
            .timeout(Duration::from_secs(20)) // a CoA can take a while to answer
            .build()
            .context("failed to build portal client")?;
        let resp = client
            .get(url)
            .send()
            .with_context(|| format!("could not reach the portal at {url}"))?;
        let mut portal = Self {
            client,
            base: resp.url().clone(),
            token: String::new(),
            sid: String::new(),
            html: String::new(),
        };
        portal.read(resp)?;
        Ok(portal)
    }

    /// Becomes the given page. ISE rotates the token through a `token` response
    /// header as well as the form fields, and the header wins.
    fn read(&mut self, resp: Response) -> Result<()> {
        self.base = resp.url().clone();
        let header_token = resp
            .headers()
            .get("token")
            .and_then(|v| v.to_str().ok())
            .map(str::to_string);
        self.html = resp.text().context("portal sent an unreadable response")?;

        let token = header_token
            .or_else(|| self.input("token"))
            .filter(|t| !t.is_empty());
        if let Some(token) = token {
            self.token = token;
        }
        if let Some(sid) = cap(r"setPortalSessionId\(\s*'([^']+)'\s*\)", &self.html) {
            self.sid = sid;
        }
        Ok(())
    }

    fn input(&self, name: &str) -> Option<String> {
        attr(tag(&self.html, "input", name)?, "value")
    }

    fn post(&self, action: &str, fields: &[(&str, &str)]) -> Result<Response> {
        let url = self
            .base
            .join(action)
            .with_context(|| format!("bad portal action {action}"))?;
        let mut form = vec![
            ("token", self.token.as_str()),
            ("portalSessionId", self.sid.as_str()),
        ];
        form.extend_from_slice(fields);
        self.client
            .post(url)
            .form(&form)
            .send()
            .with_context(|| format!("POST {action} failed"))
    }

    fn navigate(&mut self, action: &str, fields: &[(&str, &str)]) -> Result<()> {
        let resp = self.post(action, fields)?;
        self.read(resp)
    }

    fn ajax(&self, action: &str, fields: &[(&str, &str)]) -> Result<String> {
        self.post(action, fields)?
            .text()
            .with_context(|| format!("{action} sent an unreadable response"))
    }

    fn accept_aup(&mut self) -> Result<()> {
        let Some(action) = tag(&self.html, "form", "aupForm").and_then(|t| attr(t, "action")) else {
            println!("usfconnect: no AUP form on this page, it may already be accepted");
            return Ok(());
        };
        if self.token.is_empty() {
            bail!("unexpected portal page (no token)");
        }
        println!("usfconnect: accepting the AUP");
        self.navigate(&action, &[("aupAccepted", "true")])
    }

    /// Once accepted, the page asks for a CoA so the controller re-authorizes
    /// this MAC. Which endpoints to call, and whether to call them at all, come
    /// from the page itself.
    fn issue_coa(&self) -> Result<()> {
        if js_param(&self.html, "doCoA").as_deref() != Some("true") {
            return Ok(());
        }
        let param = |name, fallback: &str| {
            js_param(&self.html, name).unwrap_or_else(|| fallback.to_string())
        };
        let coa_url = param("coaUrl", "DoCoA.action");
        let poll_url = param("coaPollingUrl", "CheckCoAStatus.action");
        let coa_type = param("coaType", "Reauth");

        println!("usfconnect: requesting authorization (CoA {coa_type})");
        let body = self.ajax(
            &coa_url,
            &[
                ("delayToCoA", &param("coaDelay", "0")),
                ("coaType", &coa_type),
                ("coaSource", "GUEST"),
                ("coaReason", "Guest authenticated for network access"),
                ("waitForCoA", "true"),
            ],
        )?;
        if json_has(&body, STATUS_ERR) {
            bail!("portal refused the authorization request: {}", body.trim());
        }

        for _ in 0..10 {
            let body = self.ajax(&poll_url, &[]).unwrap_or_default();
            if json_has(&body, STATUS_OK) && json_has(&body, r#""complete""#) {
                return Ok(());
            }
            if json_has(&body, STATUS_ERR) {
                bail!("authorization failed: {}", body.trim());
            }
            std::thread::sleep(Duration::from_secs(2));
        }
        println!("usfconnect: authorization did not confirm, checking connectivity anyway");
        Ok(())
    }
}

enum Expect {
    /// A bare 204 is the only proof of an unintercepted path to the internet.
    NoContent,
    /// Body text the real site serves, so an intercepted 200 cannot pass.
    Body(&'static str),
}

struct Probe {
    url: &'static str,
    expect: Expect,
}

impl Probe {
    fn is_online(&self, status: StatusCode, body: &str) -> bool {
        match self.expect {
            Expect::NoContent => status == StatusCode::NO_CONTENT,
            Expect::Body(text) => status.is_success() && body.contains(text),
        }
    }
}

const PROBES: [Probe; 3] = [
    Probe {
        url: "http://connectivitycheck.gstatic.com/generate_204",
        expect: Expect::NoContent,
    },
    Probe {
        url: "http://detectportal.firefox.com/success.txt",
        expect: Expect::Body("success"),
    },
    Probe {
        url: "http://captive.apple.com/hotspot-detect.html",
        expect: Expect::Body("Success"),
    },
];

enum State {
    Online,
    Captive(String),
}

/// A probe that cannot be reached leaves the state *unknown*, never "online":
/// behind the portal even DNS can fail.
fn detect(client: &Client) -> Result<State> {
    let host = re(PORTAL_HOST);
    let portal_url = re(&format!(r#"https?://{PORTAL_HOST}(?::\d+)?/[^\s"'<>\\]+"#));
    let mut notes = Vec::new();

    for probe in &PROBES {
        let resp = match client.get(probe.url).send() {
            Ok(resp) => resp,
            Err(e) => {
                notes.push(format!("{}: {e}", probe.url));
                continue;
            }
        };
        let status = resp.status();
        let location = resp
            .headers()
            .get(LOCATION)
            .and_then(|v| v.to_str().ok())
            .map(str::to_string);

        if let Some(loc) = location.clone().filter(|loc| host.is_match(loc)) {
            return Ok(State::Captive(loc));
        }
        let body = resp.text().unwrap_or_default();
        // Some interceptions answer 200 with a meta-refresh or scripted redirect.
        if let Some(found) = portal_url.find(&body) {
            return Ok(State::Captive(found.as_str().to_string()));
        }
        if probe.is_online(status, &body) {
            return Ok(State::Online);
        }

        notes.push(match location {
            Some(loc) => format!("{}: HTTP {status} -> {loc}", probe.url),
            None => format!("{}: HTTP {status}", probe.url),
        });
    }

    bail!(
        "could not tell whether this machine is online or behind the portal:\n  {}\n\
         If the portal page is open in a browser, pass its URL: usfconnect.rs <portal-url>",
        notes.join("\n  ")
    )
}

fn run() -> Result<()> {
    let probe = client()
        .redirect(redirect::Policy::none())
        .build()
        .context("failed to build probe client")?;

    let gateway = match std::env::args().nth(1) {
        Some(url) => url,
        None => match detect(&probe)? {
            State::Online => {
                println!("usfconnect: already online, nothing to do");
                return Ok(());
            }
            State::Captive(url) => {
                println!("usfconnect: captive portal detected -> {url}");
                url
            }
        },
    };

    let mut portal = Portal::open(&gateway)?;
    portal.accept_aup()?;
    if let Some(message) = instruction_message(&portal.html) {
        println!("usfconnect: portal says: {message}");
    }
    portal.issue_coa()?;

    println!("usfconnect: verifying connectivity");
    for attempt in 0..10 {
        if attempt > 0 {
            std::thread::sleep(Duration::from_secs(2));
        }
        if let Ok(State::Online) = detect(&probe) {
            println!("usfconnect: connected");
            return Ok(());
        }
    }
    bail!("the AUP was submitted but this machine is still not online")
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e:#}");
        std::process::exit(1);
    }
}
