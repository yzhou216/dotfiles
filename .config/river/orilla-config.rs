#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[dependencies]
orilla = "0.1"
---
use orilla::{prelude::*, KeybindingAction};

const TAGS: &[char] = &['1', '2', '3', '4', '5', 't', 'w', 'e'];
const FULL_SHOT: &str = "wayshot - | wl-copy -t image/png; wayshot -- ~/Pictures/Screenshots/$(date +%s%N | cut -b1-13)-wayshot.png";
const REGION_SHOT: &str = "region=$(slurp); wayshot --geometry \"$region\" - | wl-copy -t image/png; wayshot --geometry \"$region\" -- ~/Pictures/Screenshots/$(date +%s%N | cut -b1-13)-wayshot.png";

fn main() {
    let tags: Vec<String> = TAGS.iter().map(|&t| t.to_string()).collect();

    orilla::Orilla::new()
        .tags(tags.clone())
        .layouts(layout_set![Tall::default(), Full])
        .wrap(Gaps::with(0))
        .borders(Borders::new(2, "#154734").focused("#00ab41"))
        .keys(
            tag_bindings(Mods::Super, action::switch_tag)
                .chain(tag_bindings(Mods::Super | Mods::Shift, action::shift_tag))
                .chain(tag_bindings(
                    Mods::Super | Mods::Shift | Mods::Ctrl,
                    action::toggle_tag,
                ))
                .chain(vec![
                    Keybinding::new(Mods::Super | Mods::Alt, 'q', action::close()),
                    Keybinding::new(Mods::Super, 'j', action::focus_next()),
                    Keybinding::new(Mods::Super, 'k', action::focus_prev()),
                    Keybinding::new(Mods::Super | Mods::Shift, 'j', action::swap_next()),
                    Keybinding::new(Mods::Super | Mods::Shift, 'k', action::swap_prev()),
                    Keybinding::new(
                        Mods::Super | Mods::Shift,
                        keysyms::Return,
                        action::promote(),
                    ),
                    Keybinding::new(Mods::Super, 'h', action::message(Tall::ShrinkMain)),
                    Keybinding::new(Mods::Super, 'l', action::message(Tall::GrowMain)),
                    Keybinding::new(
                        Mods::Super | Mods::Shift,
                        'h',
                        action::message(Tall::IncMainCount),
                    ),
                    Keybinding::new(
                        Mods::Super | Mods::Shift,
                        'l',
                        action::message(Tall::DecMainCount),
                    ),
                    Keybinding::new(Mods::Super, keysyms::Return, action::spawn("alacritty")),
                    Keybinding::new(
                        Mods::Super,
                        'd',
                        action::spawn_argv([
                            "sh",
                            "-c",
                            "pkill wmenu; wmenu-run -l 24 -N 000000ff",
                        ]),
                    ),
                    Keybinding::new(
                        Mods::Super,
                        'c',
                        action::spawn_argv(["fnottctl", "dismiss", "all"]),
                    ),
                    Keybinding::new(Mods::Super | Mods::Alt, 'l', action::spawn("wlock")),
                    Keybinding::new(
                        Mods::Super,
                        's',
                        action::spawn_argv(["sh", "-c", FULL_SHOT]),
                    ),
                    Keybinding::new(
                        Mods::Super | Mods::Shift,
                        's',
                        action::spawn_argv(["sh", "-c", REGION_SHOT]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioRaiseVolume,
                        action::spawn_argv(["pamixer", "-i", "5"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioLowerVolume,
                        action::spawn_argv(["pamixer", "-d", "5"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioMute,
                        action::spawn_argv(["pamixer", "--toggle-mute"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioPlay,
                        action::spawn_argv(["playerctl", "play-pause"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioPrev,
                        action::spawn_argv(["playerctl", "previous"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_AudioNext,
                        action::spawn_argv(["playerctl", "next"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_MonBrightnessUp,
                        action::spawn_argv(["xbacklight", "-inc", "10"]),
                    ),
                    Keybinding::new(
                        Mods::empty(),
                        keysyms::XF86_MonBrightnessDown,
                        action::spawn_argv(["xbacklight", "-dec", "10"]),
                    ),
                ])
                .collect(),
        )
        .run()
        .unwrap_or_else(|e| {
            eprintln!("orilla-config failed: {e}");
            std::process::exit(1);
        });
}

fn tag_bindings(mods: Mods, f: fn(String) -> KeybindingAction) -> impl Iterator<Item = Keybinding> {
    TAGS.iter()
        .map(move |&t| Keybinding::new(mods, t, f(t.to_string())))
}
