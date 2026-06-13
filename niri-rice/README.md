# niri rice profile

This profile installs the niri desktop setup used on the Linux workstation:

- niri config with named workspaces, themed backdrop/focus ring, workspace pickers, and theme switching
- Alacritty themes and `Mod+Shift+T` theme switcher, plus optional Ghostty/Foot terminal configs
- Waybar top bar with niri workspaces, taskbar, focused window, compact system pill, battery, clock, and tray
- fuzzel launcher and workspace/task pickers
- scratchpad terminal, screenshot picker, and lightweight clipboard history picker
- mako notification styling and auto-dismiss behavior
- tmux truecolor/default-color passthrough
- bash/fish themed shell prompt
- Emacs `init.el`, including `doom-nord` and Rust mode support
- GTK dark mode/icon defaults and Copilot CLI dark/default color mode

## Install

```bash
git clone https://github.com/anakrish/dotfiles.git
cd dotfiles
./niri-rice/install.sh --install-packages
```

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
| `Mod+Shift+D` | Jump to workspace |
| `Mod+Ctrl+D` | Move focused window to workspace |
| `Mod+Shift+N` | Rename current workspace |
| `Mod+Grave` | Focus/create scratchpad terminal |
| `Mod+Shift+S` | Screenshot picker |
| `Mod+Shift+C` | Clipboard history picker |

## Recommended packages

The installer can install these with `--install-packages` on apt, dnf, or pacman systems when package names are available:

```text
niri waybar fuzzel mako alacritty ghostty foot tmux fish emacs brightnessctl playerctl pavucontrol grim slurp wl-clipboard
```

Some distributions may package `mako` as `mako-notifier`, and some may not ship `niri` in the default repositories.
