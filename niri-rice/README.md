# niri rice profile

This profile installs the niri desktop setup used on the Linux workstation:

- niri config with named workspaces, themed backdrop/focus ring, workspace pickers, and theme switching
- Alacritty themes and `Mod+Shift+T` theme switcher, plus optional Ghostty/Foot terminal configs
- Waybar top bar config with niri workspaces, taskbar, focused window, compact system pill, battery, clock, and tray
- fuzzel launcher and workspace/task pickers
- scratchpad terminal, screenshot picker, and lightweight clipboard history picker
- mako notification styling and auto-dismiss behavior
- tmux truecolor/default-color passthrough
- bash/fish themed shell prompt
- Emacs `init.el`, including `doom-nord` and Rust mode support
- GTK dark mode/icon defaults and Copilot CLI dark/default color mode

The niri config does not start Waybar directly. This avoids duplicate bars on systems where the niri session or distro already starts Waybar; the installed `~/.config/waybar` files make that existing Waybar use the rice styling.

## Install

```bash
git clone https://github.com/anakrish/dotfiles.git
cd dotfiles
./niri-rice/install.sh --install-packages
```

If your distro does not provide a recent niri package, build and install niri from source first:

```bash
./niri-rice/install.sh --build-niri --install-packages
```

`--build-niri` installs niri build dependencies, installs/updates stable Rust with rustup when needed, clones niri into `~/.cache/niri-rice/niri`, builds `cargo build --release`, and installs niri resources under `/usr/local`. To build a specific branch, tag, or commit:

```bash
./niri-rice/install.sh --build-niri --niri-ref v25.11
```

### XWayland support (`--xwayland-satellite`)

niri has no built-in XWayland, so X11-only apps (for example Git Credential Manager's
GUI prompt) have no display under a bare niri session. Install
[`xwayland-satellite`](https://github.com/Supreeeme/xwayland-satellite), a rootless
XWayland server, with:

```bash
./niri-rice/install.sh --xwayland-satellite
```

This uses a distro package when one is available and otherwise builds it from source
with cargo (installing stable Rust via rustup when needed) and symlinks the binary into
`~/.local/bin`. The niri config spawns it at startup on display `:0` and exports a
matching `DISPLAY`, so X11 clients work automatically.

The installer backs up replaced files under:

```text
~/.dotfiles-backup/niri-rice-YYYYMMDD-HHMMSS
```

If you already installed the dependencies, run without package installation:

```bash
./niri-rice/install.sh
```

## Main shortcuts

| Shortcut | Action |
|---|---|
| `Mod+T` | Open Alacritty |
| `Mod+Shift+T` | Switch desktop/terminal theme |
| `Mod+D` | Open fuzzel app launcher |
| `Mod+-` | Jump to/create web workspace |
| `Mod+=` | Jump to/create chat workspace |
| `Mod+Shift+D` | Jump to workspace |
| `Mod+Ctrl+D` | Move focused window to workspace |
| `Mod+Shift+N` | Rename current workspace |
| `Mod+Grave` | Toggle scratchpad terminal (show/hide) |
| `Mod+Shift+S` | Screenshot picker |
| `Mod+Shift+C` | Clipboard history picker |

## Recommended packages

The installer can install these with `--install-packages` on apt, dnf, or pacman systems when package names are available:

```text
niri waybar fuzzel mako alacritty ghostty foot tmux fish emacs brightnessctl playerctl pavucontrol grim slurp wl-clipboard
```

Some distributions may package `mako` as `mako-notifier`, and some may not ship `niri` in the default repositories.

### Fonts

The terminal, waybar, and shell-prompt configs use Nerd Font patched families
(`JetBrainsMono Nerd Font Mono` and `Hack Nerd Font Mono`) for powerline and icon
glyphs. Since most distributions do not package the Nerd-patched variants, `install.sh`
downloads them from the upstream [nerd-fonts](https://github.com/ryanoasis/nerd-fonts)
release into `~/.local/share/fonts` (skipping any already present) and refreshes the font
cache. This runs automatically on every install, no flag required.

## Building niri from source

The `--build-niri` path follows niri's upstream manual install layout:

| Source file | Installed path |
|---|---|
| `target/release/niri` | `/usr/local/bin/niri` |
| `resources/niri-session` | `/usr/local/bin/niri-session` |
| `resources/niri.desktop` | `/usr/local/share/wayland-sessions/niri.desktop` |
| `resources/niri-portals.conf` | `/usr/local/share/xdg-desktop-portal/niri-portals.conf` |
| `resources/niri.service` | `/etc/systemd/user/niri.service` |
| `resources/niri-shutdown.target` | `/etc/systemd/user/niri-shutdown.target` |
