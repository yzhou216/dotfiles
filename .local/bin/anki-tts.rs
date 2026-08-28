#!/usr/bin/env -S cargo +nightly -Zscript
---
[package]
edition = "2024"
[dependencies]
clap = { version = "4", features = ["derive"] }
anyhow = "1"
rusqlite = { version = "0.37", features = ["bundled", "collation"] }
rayon = "1"
html-escape = "0.2"
---
use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use html_escape::decode_html_entities;
use rayon::prelude::*;
use rusqlite::{params, Connection};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

const DEFAULT_COL: &str = ".local/share/Anki2/User 1/collection.anki2";
const DEFAULT_JOBS: usize = 32;

fn main() {
    if let Err(e) = run() {
        let msg = format!("{:#}", e);
        eprintln!("Error: {}", msg);
        let lower = msg.to_lowercase();
        if lower.contains("database is locked") || lower.contains("busy") {
            eprintln!("The Anki collection appears to be open.  Close Anki and run again.");
        }
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let args = Args::parse();
    let col_path = args.col_path();
    let media_dir = args.media_dir();
    println!(
        "Collection: {}\nMedia: {}",
        col_path.display(),
        media_dir.display()
    );

    let conn = open_collection(&col_path)?;
    let deck_ids = resolve_decks(&conn, &args.deck)?;
    let nids = note_ids_in_decks(&conn, &deck_ids)?;
    if nids.is_empty() {
        println!("No notes in deck(s) {:?}", args.deck);
        return Ok(());
    }
    println!("Found {} note(s) in deck(s) {:?}", nids.len(), args.deck);

    let by_mid = group_by_notetype(&conn, &nids)?;
    let tmp_dir = std::env::temp_dir().join(format!("anki-tts-{}", std::process::id()));
    std::fs::create_dir_all(&tmp_dir).context("failed to create temp dir")?;
    if !media_dir.exists() && !args.dry_run {
        std::fs::create_dir_all(&media_dir).context("failed to create media dir")?;
    }

    let groups: Vec<(Vec<Job>, usize)> = by_mid
        .iter()
        .map(|(mid, mid_nids)| {
            println!("Notetype {}: processing {} note(s)", mid, mid_nids.len());
            let names = field_names(&conn, *mid)?;
            let (audio_ord, msgs) = ensure_audio_field(&conn, *mid, &args.audio, args.dry_run)?;
            for m in msgs {
                println!("  [dry-run] {}", m);
            }
            let field_count = if audio_ord >= names.len() {
                names.len() + 1
            } else {
                names.len()
            };
            let source_ord = resolve_source_ord(&names, &args.field)?;
            let classified: Vec<Pending> = mid_nids
                .iter()
                .map(|&nid| classify_note(&conn, nid, source_ord, audio_ord, field_count))
                .collect::<Result<Vec<_>>>()?;
            for nid in classified.iter().filter_map(|p| match p {
                Pending::Empty(nid) => Some(nid),
                _ => None,
            }) {
                println!("  note {}: empty source text, skipping", nid);
            }
            let skipped = classified
                .iter()
                .filter(|p| matches!(p, Pending::Already | Pending::Empty(_)))
                .count();
            let jobs = classified
                .into_iter()
                .filter_map(|p| match p {
                    Pending::Job(job) => Some(job),
                    _ => None,
                })
                .collect();
            Ok::<_, anyhow::Error>((jobs, skipped))
        })
        .collect::<Result<Vec<_>>>()?;

    let jobs: Vec<Job> = groups
        .iter()
        .flat_map(|(jobs, _)| jobs.iter().cloned())
        .collect();
    let skipped: usize = groups.iter().map(|(_, skipped)| skipped).sum();
    let to_generate = jobs.len();

    if args.dry_run {
        for job in &jobs {
            println!("  [dry-run] note {}: would TTS '{}'", job.nid, job.text);
        }
        let _ = std::fs::remove_dir_all(&tmp_dir);
        println!(
            "Done: would generate {} audio file(s), {} already had audio.",
            to_generate, skipped
        );
        return Ok(());
    }

    let synth = Synth {
        voice: args.voice.clone(),
        format: args.format.clone(),
    };
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(args.jobs)
        .build()
        .context("failed to build thread pool")?;
    let results: Vec<(Job, PathBuf)> = pool.install(|| {
        jobs.par_iter()
            .map(|job| {
                synth
                    .synthesize(job, &tmp_dir)
                    .map(|file| (job.clone(), file))
            })
            .collect::<Result<Vec<_>>>()
    })?;

    for (job, file) in results {
        let dest = media_dir.join(file.file_name().unwrap());
        std::fs::copy(&file, &dest).context("failed to copy audio to media dir")?;
        let tag = sound_tag(job.nid, &args.format);
        update_note_audio(&conn, &job, &tag)?;
        println!("  note {}: added audio ({})", job.nid, tag);
    }

    let _ = std::fs::remove_dir_all(&tmp_dir);
    println!(
        "Done: {} audio generated, {} already had audio.",
        to_generate, skipped
    );
    Ok(())
}

#[derive(Parser)]
#[command(name = "anki-tts", about = "Add edge-tts audio to Anki cards")]
struct Args {
    #[arg(
        short,
        long,
        required = true,
        help = "deck(s) to process; 'all' = every deck"
    )]
    deck: Vec<String>,

    #[arg(short, long, help = "edge-tts voice, e.g. de-DE-ConradNeural")]
    voice: String,

    #[arg(
        long,
        help = "path to collection.anki2 (default: ~/.local/share/Anki2/User 1/collection.anki2)"
    )]
    col: Option<PathBuf>,

    #[arg(
        long,
        help = "source field to synthesize (default: Front, else first field)"
    )]
    field: Option<String>,

    #[arg(
        long,
        default_value = "ogg",
        value_parser = ["ogg", "opus"],
        help = "audio format: ogg (Vorbis) or opus"
    )]
    format: String,

    #[arg(long, default_value = "Audio", help = "audio field name")]
    audio: String,

    #[arg(
        long,
        help = "collection.media directory (default: next to collection.anki2)"
    )]
    media: Option<PathBuf>,

    #[arg(long, default_value_t = DEFAULT_JOBS, help = "max concurrent TTS jobs (0 = auto)")]
    jobs: usize,

    #[arg(long, help = "show what would be done without changing anything")]
    dry_run: bool,
}

impl Args {
    fn col_path(&self) -> PathBuf {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/".to_string());
        self.col
            .clone()
            .unwrap_or_else(|| PathBuf::from(&home).join(DEFAULT_COL))
    }

    fn media_dir(&self) -> PathBuf {
        self.media.clone().unwrap_or_else(|| {
            self.col_path()
                .parent()
                .map(|p| p.join("collection.media"))
                .unwrap_or_else(|| PathBuf::from("collection.media"))
        })
    }
}

#[derive(Clone)]
struct Job {
    nid: i64,
    text: String,
    flds: String,
    audio_ord: usize,
    field_count: usize,
}

enum Pending {
    Job(Job),
    Already,
    Empty(i64),
}

fn sound_tag(nid: i64, format: &str) -> String {
    format!("[sound:{}.{}]", nid, format)
}

struct Synth {
    voice: String,
    format: String,
}

fn run_cmd(cmd: &mut std::process::Command, program: &str) -> Result<()> {
    let output = cmd
        .output()
        .with_context(|| format!("failed to run {program} (is it installed and on PATH?)"))?;
    if !output.status.success() {
        bail!(
            "{program} failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(())
}

impl Synth {
    fn synthesize(&self, job: &Job, tmp_dir: &Path) -> Result<PathBuf> {
        let base = tmp_dir.join(job.nid.to_string());
        let mp3 = base.with_extension("mp3");
        let out = base.with_extension(&self.format);

        let mut edge_tts = std::process::Command::new("edge-tts");
        edge_tts.args([
            "--voice",
            &self.voice,
            "--text",
            &job.text,
            "--write-media",
            mp3.to_str().unwrap(),
        ]);
        run_cmd(&mut edge_tts, "edge-tts")?;

        let mut ffmpeg = std::process::Command::new("ffmpeg");
        ffmpeg.args(["-y", "-loglevel", "error", "-i", mp3.to_str().unwrap()]);
        if self.format == "ogg" {
            ffmpeg.args(["-codec:a", "libvorbis", "-qscale:a", "4"]);
        } else {
            ffmpeg.args(["-codec:a", "libopus", "-b:a", "64k"]);
        }
        ffmpeg.arg(out.to_str().unwrap());
        run_cmd(&mut ffmpeg, "ffmpeg")?;
        let _ = std::fs::remove_file(&mp3);
        Ok(out)
    }
}

fn open_collection(path: &Path) -> Result<Connection> {
    if !path.exists() {
        bail!("collection not found: {}", path.display());
    }
    let conn =
        Connection::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    conn.busy_timeout(std::time::Duration::from_millis(300))?;
    conn.create_collation("unicase", |a: &str, b: &str| -> Ordering {
        a.to_lowercase().cmp(&b.to_lowercase())
    })?;
    Ok(conn)
}

fn deck_matches(name: &str, target: &str) -> bool {
    name == target || name.starts_with(&format!("{}::", target))
}

fn resolve_decks(conn: &Connection, targets: &[String]) -> Result<Vec<i64>> {
    let mut stmt = conn
        .prepare("SELECT id, name FROM decks")
        .context("failed to query decks")?;
    let all: Vec<(i64, String)> = stmt
        .query_map([], |r| Ok((r.get(0)?, r.get(1)?)))
        .context("failed to read decks")?
        .collect::<Result<_, _>>()
        .context("failed to read decks")?;
    targets
        .iter()
        .filter(|t| t.as_str() != "all")
        .try_for_each(|t| {
            if all.iter().any(|(_, name)| deck_matches(name, t)) {
                Ok(())
            } else {
                bail!("deck '{}' not found", t)
            }
        })?;
    let ids: Vec<i64> = targets
        .iter()
        .flat_map(|t| {
            if t == "all" {
                all.iter().map(|(id, _)| *id).collect::<Vec<i64>>()
            } else {
                all.iter()
                    .filter(|(_, name)| deck_matches(name, t))
                    .map(|(id, _)| *id)
                    .collect::<Vec<i64>>()
            }
        })
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    Ok(ids)
}

fn note_ids_in_decks(conn: &Connection, deck_ids: &[i64]) -> Result<Vec<i64>> {
    let placeholders = vec!["?"; deck_ids.len()].join(",");
    let sql = format!(
        "SELECT DISTINCT nid FROM cards WHERE did IN ({}) ORDER BY nid",
        placeholders
    );
    let mut stmt = conn.prepare(&sql).context("failed to query notes")?;
    stmt.query_map(rusqlite::params_from_iter(deck_ids.iter()), |r| {
        r.get::<_, i64>(0)
    })
    .context("failed to query notes")?
    .collect::<Result<_, _>>()
    .context("failed to query notes")
}

fn field_names(conn: &Connection, ntid: i64) -> Result<Vec<String>> {
    let mut stmt = conn
        .prepare("SELECT name FROM fields WHERE ntid = ?1 ORDER BY ord")
        .context("failed to query fields")?;
    stmt.query_map(params![ntid], |r| r.get::<_, String>(0))
        .context("failed to query fields")?
        .collect::<Result<_, _>>()
        .context("failed to query fields")
}

fn group_by_notetype(conn: &Connection, nids: &[i64]) -> Result<HashMap<i64, Vec<i64>>> {
    nids.iter()
        .try_fold(HashMap::<i64, Vec<i64>>::new(), |mut groups, &nid| {
            let mid: i64 = conn
                .query_row("SELECT mid FROM notes WHERE id = ?1", params![nid], |r| {
                    r.get(0)
                })
                .context("failed to read note")?;
            groups.entry(mid).or_default().push(nid);
            Ok(groups)
        })
}

fn resolve_source_ord(names: &[String], field: &Option<String>) -> Result<usize> {
    match field {
        Some(f) => names
            .iter()
            .position(|n| n.eq_ignore_ascii_case(f))
            .ok_or_else(|| anyhow!("field '{}' not found in notetype", f)),
        None => Ok(names
            .iter()
            .position(|n| n.eq_ignore_ascii_case("Front"))
            .unwrap_or(0)),
    }
}

fn ensure_audio_field(
    conn: &Connection,
    ntid: i64,
    audio_field: &str,
    dry_run: bool,
) -> Result<(usize, Vec<String>)> {
    let names = field_names(conn, ntid)?;
    if let Some(idx) = names
        .iter()
        .position(|n| n.eq_ignore_ascii_case(audio_field))
    {
        return Ok((idx, Vec::new()));
    }
    let ord = names.len() as i64;
    if !dry_run {
        let config = build_field_config(random_id());
        conn.execute(
            "INSERT INTO fields (ntid, ord, name, config) VALUES (?1,?2,?3,?4)",
            params![ntid, ord, audio_field, config],
        )
        .context("failed to add audio field")?;
    }
    let mut stmt = conn
        .prepare("SELECT ord, config FROM templates WHERE ntid = ?1")
        .context("failed to query templates")?;
    let rows: Vec<(i64, Vec<u8>)> = stmt
        .query_map(params![ntid], |r| Ok((r.get(0)?, r.get(1)?)))
        .context("failed to query templates")?
        .collect::<Result<_, _>>()
        .context("failed to query templates")?;
    let needs_audio = |config: &[u8]| !config.windows(9).any(|w| w == b"{{Audio}}");
    let template_msgs: Vec<String> = if dry_run {
        rows.iter()
            .filter(|(_, config)| needs_audio(config))
            .map(|(tord, _)| {
                format!(
                    "would add {{Audio}} to template {} of notetype {}",
                    tord, ntid
                )
            })
            .collect()
    } else {
        Vec::new()
    };
    if !dry_run {
        for (tord, config) in rows.iter().filter(|(_, config)| needs_audio(config)) {
            let qfmt = get_string_field(config, 1).unwrap_or_default();
            let new_qfmt = format!("{}<br>{{{{Audio}}}}", qfmt);
            let new_config = replace_string_field(config, 1, new_qfmt.as_bytes());
            conn.execute(
                "UPDATE templates SET config = ?1 WHERE ntid = ?2 AND ord = ?3",
                params![new_config, ntid, tord],
            )
            .context("failed to update template")?;
        }
        conn.execute(
            "UPDATE notetypes SET mtime_secs = ?1 WHERE id = ?2",
            params![now_secs(), ntid],
        )
        .context("failed to update notetype")?;
        let now = now_ms();
        conn.execute("UPDATE col SET mod = ?1, scm = ?1", params![now])
            .context("failed to update collection")?;
    }
    let msgs: Vec<String> = dry_run
        .then(|| format!("would add field '{}' to notetype {}", audio_field, ntid))
        .into_iter()
        .chain(template_msgs)
        .collect();
    Ok((ord as usize, msgs))
}

fn classify_note(
    conn: &Connection,
    nid: i64,
    source_ord: usize,
    audio_ord: usize,
    field_count: usize,
) -> Result<Pending> {
    let flds: String = conn
        .query_row("SELECT flds FROM notes WHERE id = ?1", params![nid], |r| {
            r.get(0)
        })
        .context("failed to read note fields")?;
    let parts: Vec<&str> = flds.split('\x1f').collect();
    if parts.get(audio_ord).is_some_and(|v| v.contains("[sound:")) {
        return Ok(Pending::Already);
    }
    let text = strip_html(parts.get(source_ord).copied().unwrap_or(""))
        .trim()
        .to_string();
    if text.is_empty() {
        return Ok(Pending::Empty(nid));
    }
    Ok(Pending::Job(Job {
        nid,
        text,
        flds,
        audio_ord,
        field_count,
    }))
}

fn update_note_audio(conn: &Connection, job: &Job, sound_tag: &str) -> Result<()> {
    let fields: Vec<String> = job.flds.split('\x1f').map(String::from).collect();
    let len = fields.len().max(job.field_count);
    let new_flds: String = (0..len)
        .map(|i| {
            if i == job.audio_ord {
                sound_tag.to_string()
            } else {
                fields.get(i).cloned().unwrap_or_default()
            }
        })
        .collect::<Vec<_>>()
        .join("\x1f");
    conn.execute(
        "UPDATE notes SET flds = ?1, mod = ?2, usn = -1 WHERE id = ?3",
        params![new_flds, now_secs(), job.nid],
    )
    .context("failed to update note")?;
    Ok(())
}

fn strip_html(s: &str) -> String {
    let mut stripped = String::new();
    let mut in_tag = false;
    for c in s.chars() {
        if c == '<' {
            in_tag = true;
        } else if c == '>' {
            in_tag = false;
        } else if !in_tag {
            stripped.push(c);
        }
    }
    decode_html_entities(&stripped).replace('\u{a0}', " ")
}

fn proto_varint(mut n: u64) -> Vec<u8> {
    let mut out = Vec::new();
    loop {
        let b = (n & 0x7f) as u8;
        n >>= 7;
        if n != 0 {
            out.push(b | 0x80);
        } else {
            out.push(b);
            break;
        }
    }
    out
}

fn proto_len_delim(field: u64, data: &[u8]) -> Vec<u8> {
    [
        proto_varint((field << 3) | 2),
        proto_varint(data.len() as u64),
        data.to_vec(),
    ]
    .concat()
}

fn proto_varint_field(field: u64, n: u64) -> Vec<u8> {
    [proto_varint((field << 3) | 0), proto_varint(n)].concat()
}

fn read_varint(data: &[u8], i: &mut usize) -> u64 {
    let mut value = 0u64;
    let mut shift = 0;
    loop {
        let byte = data[*i];
        *i += 1;
        value |= ((byte & 0x7f) as u64) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    value
}

fn get_string_field(data: &[u8], target: u64) -> Option<String> {
    let mut i = 0;
    while i < data.len() {
        let key = read_varint(data, &mut i);
        let field = key >> 3;
        match key & 7 {
            2 => {
                let len = read_varint(data, &mut i) as usize;
                let value = &data[i..i + len];
                i += len;
                if field == target {
                    return String::from_utf8(value.to_vec()).ok();
                }
            }
            0 => {
                read_varint(data, &mut i);
            }
            1 => i += 8,
            5 => i += 4,
            _ => return None,
        }
    }
    None
}

fn replace_string_field(data: &[u8], target: u64, new_value: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    let mut found = false;
    let mut i = 0;
    while i < data.len() {
        let key_start = i;
        let key = read_varint(data, &mut i);
        let field = key >> 3;
        match key & 7 {
            2 => {
                let len = read_varint(data, &mut i) as usize;
                i += len;
                if field == target && !found {
                    out.extend(proto_varint((target << 3) | 2));
                    out.extend(proto_varint(new_value.len() as u64));
                    out.extend(new_value);
                    found = true;
                } else {
                    out.extend(&data[key_start..i]);
                }
            }
            0 => {
                read_varint(data, &mut i);
                out.extend(&data[key_start..i]);
            }
            1 => {
                i += 8;
                out.extend(&data[key_start..i]);
            }
            5 => {
                i += 4;
                out.extend(&data[key_start..i]);
            }
            _ => return data.to_vec(),
        }
    }
    if !found {
        out.extend(proto_varint((target << 3) | 2));
        out.extend(proto_varint(new_value.len() as u64));
        out.extend(new_value);
    }
    out
}

fn build_field_config(field_id: i64) -> Vec<u8> {
    [
        proto_len_delim(3, b"Arial"),
        proto_varint_field(4, 20),
        proto_varint_field(9, field_id as u64),
    ]
    .concat()
}

fn now_secs() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

fn now_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

fn random_id() -> i64 {
    let ns = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i64)
        .unwrap_or(0);
    ns ^ ((std::process::id() as i64) << 32)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn varint_field(data: &[u8], target: u64) -> Option<u64> {
        let mut i = 0;
        while i < data.len() {
            let key = read_varint(data, &mut i);
            let field = key >> 3;
            match key & 7 {
                0 => {
                    let value = read_varint(data, &mut i);
                    if field == target {
                        return Some(value);
                    }
                }
                2 => {
                    let len = read_varint(data, &mut i) as usize;
                    i += len;
                }
                1 => i += 8,
                5 => i += 4,
                _ => return None,
            }
        }
        None
    }

    #[test]
    fn sound_tag_embeds_note_id_and_format() {
        assert_eq!(sound_tag(42, "ogg"), "[sound:42.ogg]");
        assert_eq!(sound_tag(42, "opus"), "[sound:42.opus]");
    }

    #[test]
    fn strip_html_removes_tags_and_unescapes_entities() {
        assert_eq!(strip_html("Guten <b>Morgen</b>.<br>"), "Guten Morgen.");
        assert_eq!(
            strip_html("&lt;x&gt; &amp; &quot;q&quot; &#39;s&#39; a&nbsp;b"),
            "<x> & \"q\" 's' a b"
        );
    }

    #[test]
    fn run_cmd_reports_failures() {
        let mut ok = std::process::Command::new("/bin/sh");
        ok.args(["-c", "exit 0"]);
        assert!(run_cmd(&mut ok, "sh").is_ok());
        let mut bad = std::process::Command::new("/bin/sh");
        bad.args(["-c", "exit 1"]);
        let err = run_cmd(&mut bad, "sh").unwrap_err();
        assert!(err.to_string().contains("sh failed"));
    }

    #[test]
    fn field_config_contains_font_size_and_id() {
        let config = build_field_config(123456789);
        assert_eq!(get_string_field(&config, 3).as_deref(), Some("Arial"));
        assert_eq!(varint_field(&config, 4), Some(20));
        assert_eq!(varint_field(&config, 9), Some(123456789));
    }

    #[test]
    fn replace_qfmt_preserves_other_fields() {
        let mut config = proto_len_delim(1, b"{{Front}}");
        config.extend(proto_len_delim(2, b"{{Back}}"));
        config.extend(proto_varint_field(8, 777));
        let updated = replace_string_field(&config, 1, b"{{Front}}<br>{{Audio}}");
        assert_eq!(
            get_string_field(&updated, 1).as_deref(),
            Some("{{Front}}<br>{{Audio}}")
        );
        assert_eq!(get_string_field(&updated, 2).as_deref(), Some("{{Back}}"));
        assert_eq!(varint_field(&updated, 8), Some(777));
    }

    #[test]
    fn replace_qfmt_appends_when_field_missing() {
        let config = proto_varint_field(8, 1);
        let updated = replace_string_field(&config, 1, b"{{Front}}<br>{{Audio}}");
        assert_eq!(
            get_string_field(&updated, 1).as_deref(),
            Some("{{Front}}<br>{{Audio}}")
        );
        assert_eq!(varint_field(&updated, 8), Some(1));
    }

    #[test]
    fn source_ord_resolves_named_field_or_front() {
        let names = vec!["Front".to_string(), "Back".to_string()];
        assert_eq!(resolve_source_ord(&names, &None).unwrap(), 0);
        assert_eq!(
            resolve_source_ord(&names, &Some("Back".to_string())).unwrap(),
            1
        );
        assert!(resolve_source_ord(&names, &Some("Missing".to_string())).is_err());
    }

    #[test]
    fn deck_matches_exact_names_and_subdecks() {
        assert!(deck_matches("Sag Mal", "Sag Mal"));
        assert!(deck_matches("Sag Mal::Chapter 1", "Sag Mal"));
        assert!(!deck_matches("Sag Malik", "Sag Mal"));
        assert!(!deck_matches("Sag", "Sag Mal"));
    }
}
