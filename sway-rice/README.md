# sway-rice profile

Sway tiling Wayland compositor profile — the companion to `niri-rice` for
machines where niri cannot run (Hyper-V VMs, hardware without proper DRM
exposure).  Ships the same tooling, waybar styling, fuzzel launcher, clipboard
history daemon, scratchpad terminal, and theme switcher; keybindings mirror
niri-rice so switching between the two profiles requires no relearning.

## Why sway instead of niri

niri requires direct DRM/KMS access (`/dev/dri/card*`).  Under Hyper-V — and
many other VMs — the virtual GPU is not exposed as a proper DRM device, so
niri fails with `os error 11` (issue niri-wm/niri#3527).  sway uses wlroots,
which has a Pixman software-rendering fallback that works on Hyper-V with
`WLR_RENDERER=pixman`.

## Install

```bash
git clone https://github.com/anakrish/dotfiles.git
cd dotfiles
./sway-rice/install.sh --install-packages
```

The installer also registers a `Sway (Rice)` Wayland session `.desktop` that
sets `WLR_RENDERER=pixman` and `WLR_NO_HARDWARE_CURSORS=1` automatically —
pick it from GDM, SDDM, or any display manager that reads
`/usr/local/share/wayland-sessions/`.

If you already have the packages, run without `--install-packages`:

```bash
./sway-rice/install.sh
```

### Hyper-V / VM rendering

On a Hyper-V Enhanced Session (Ubuntu + xrdp + GNOME) you can run sway in a
terminal from within GNOME (nested) or as a standalone session from a TTY /
display manager.

**Nested inside GNOME** (quick try, no reboot):
```bash
WLR_RENDERER=pixman sway
```
Keybindings use `$mod = Super` natively and `Alt` when nested.

**Standalone (as your desktop)**:
Log out and select `Sway (Rice)` from the login screen, or from a TTY:
```bash
WLR_RENDERER=pixman WLR_NO_HARDWARE_CURSORS=1 sway
```

> Note: Hyper-V Enhanced Session is XRDP-based (X11 only). Running sway
> standalone through GDM/SDDM on a local TTY works fine. If you want to
> remote-desktop *into* the sway session over RDP, consider replacing the
> Enhanced Session XRDP setup with `wayvnc` or `gnome-remote-desktop`, which
> support Wayland compositors natively.

## Main shortcuts

All binds use `Super` (`$mod`), matching niri-rice muscle memory.

| Shortcut | Action |
|---|---|
| `Super+T` | Open GNOME Terminal |
| `Super+Shift+T` | Switch desktop/terminal theme |
| `Super+D` | Open fuzzel app launcher |
| `Super+Minus` | Jump to/create **web** workspace |
| `Super+Equal` | Jump to/create **chat** workspace |
| `Super+Shift+D` | Workspace picker (fuzzel) |
| `Super+Ctrl+D` | Move window to workspace (fuzzel) |
| `Super+Shift+N` | Rename current workspace |
| `Super+Grave` | Toggle scratchpad terminal |
| `Super+Shift+S` | Screenshot picker |
| `Super+Shift+C` | Clipboard history picker |
| `Super+H/J/K/L` | Focus window left/down/up/right |
| `Super+Ctrl+H/J/K/L` | Move window left/down/up/right |
| `Super+Shift+H/J/K/L` | Focus output (monitor) |
| `Super+U / Super+I` | Workspace prev / next |
| `Super+1…9` | Switch to workspace 1–9 |
| `Super+Ctrl+1…9` | Move window to workspace 1–9 |
| `Super+F` | Fullscreen toggle |
| `Super+R` | Resize mode |
| `Super+Shift+E` | Cycle layout (split / tabbed / stacking) |
| `Super+Shift+R` | Reload sway config |
| `Super+Shift+Ctrl+L` | Lock screen (swaylock) |

## Recommended packages

Installed automatically with `--install-packages` on apt, dnf, or pacman:

```text
sway waybar fuzzel mako gnome-terminal foot tmux fish emacs
brightnessctl playerctl pavucontrol grim slurp wl-clipboard
swayidle swaylock xwayland
```

### Fonts

Terminal, waybar, and shell-prompt configs use Nerd Font patched families
(`JetBrainsMono Nerd Font Mono`, `Hack Nerd Font Mono`).  The installer
downloads them from the upstream nerd-fonts release into `~/.local/share/fonts`
(skipping families already present) and refreshes the font cache.

## Theme switching

`Super+Shift+T` opens a fuzzel theme picker.  Switching a theme updates:

- GNOME Terminal palette + colors (via dconf)
- sway output background and `client.focused` border colours (`swaymsg reload`)
- Waybar colour palette (`colors.css`, live-reloaded via `SIGUSR2`)
- mako notification colours (reloaded via `makoctl reload`)
- fuzzel colours
- Shell prompt RGB variables (`.config/shell-prompt/colors.{sh,fish}`)
- GTK theme and icon theme via `gsettings`
- Ghostty theme line (if Ghostty config is present)

Available themes: `nord`, `catppuccin-mocha`, `tokyo-night`, `gruvbox-dark`,
`dracula`, `kanagawa`, `everforest-dark`, `one-dark`, `ayu-mirage`,
`rose-pine-moon`, `solarized-dark`.

## Differences from niri-rice

| Feature | niri-rice | sway-rice |
|---|---|---|
| Compositor | niri (scrollable tiling) | sway (i3-compatible tiling) |
| Workspace model | Dynamic, named + numbered | Static 1–9 + named |
| Scratchpad | Custom toggle script (niri IPC) | Custom toggle script (sway IPC) |
| Bar workspaces | Custom `waybar-niri-workspaces` script | Built-in `sway/workspaces` waybar module |
| Focused window title | Custom `waybar-niri-window` script | Built-in `sway/window` waybar module |
| Hyper-V / VM | ❌ DRM device not available | ✅ `WLR_RENDERER=pixman` fallback |
| XWayland | xwayland-satellite (rootless) | Built into sway |

The installer backs up replaced files under:

```text
~/.dotfiles-backup/sway-rice-YYYYMMDD-HHMMSS
```
