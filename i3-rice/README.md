# i3-rice — Nord-themed i3 profile for Hyper-V / xrdp

> **xrdp Enhanced Session companion** to `niri-rice`. Mirrors keybindings, Nord theme system, and visual style — no Wayland required.

## Requirements

- Ubuntu 22.04+ in a Hyper-V VM accessed via Windows `mstsc` (RDP / xrdp Enhanced Session)
- i3-gaps, polybar, rofi, dunst, picom, alacritty

## Install

```bash
./i3-rice/install.sh [--install-packages]
```

`--install-packages` runs `apt-get install` for all recommended packages first.

After installing, **log out and select i3** at the xrdp login screen.

---

## Keybindings

| Key | Action |
|-----|--------|
| `Super+T` | New Alacritty terminal |
| `Super+Shift+T` | Switch Alacritty/system theme |
| `Super+D` | App launcher (rofi) |
| `Super+Shift+D` | Workspace picker — focus |
| `Super+Ctrl+D` | Workspace picker — move window |
| `Super+Shift+N` | Rename current workspace |
| `Super+\`` | Toggle scratchpad terminal (tmux) |
| `Super+Shift+S` | Screenshot picker |
| `Super+Shift+C` | Clipboard history picker |
| `Super+B` | Browser (microsoft-edge) |
| `Super+E` | Files (nautilus) |
| `Super+Q` | Close window |
| `Super+H/J/K/L` | Focus left/down/up/right |
| `Super+Ctrl+H/J/K/L` | Move window left/down/up/right |
| `Super+Shift+H/J/K/L` | Focus output/monitor |
| `Super+F` | Fullscreen toggle |
| `Super+Shift+F` | Fullscreen global |
| `Super+Shift+E` | Cycle layout (split / tabbed / stacking) |
| `Super+Shift+Space` | Toggle floating |
| `Super+Space` | Focus toggle (tiling ↔ floating) |
| `Super+U / Super+I` | Previous / next workspace |
| `Super+Page_Up/Down` | Previous / next workspace |
| `Super+Ctrl+U/I` | Move window to prev/next workspace |
| `Super+Minus` | Quick-jump to `web` workspace |
| `Super+Equal` | Quick-jump to `chat` workspace |
| `Super+1..9` | Switch to workspace 1–9 |
| `Super+Ctrl+1..9` | Move window to workspace 1–9 |
| `Super+R` → resize | Resize mode (H/J/K/L or arrows) |
| `Super+Shift+Ctrl+L` | Lock screen (i3lock) |
| `Super+Shift+R` | Reload i3 config |
| `Super+Shift+Ctrl+E` | Exit i3 (confirmation prompt) |

---

## Theme switching

```bash
alacritty-theme-switch nord
alacritty-theme-switch catppuccin-mocha
alacritty-theme-switch tokyo-night
# … 11 themes total
```

Or press `Super+Shift+T` to pick interactively with rofi.

Switching a theme updates: Alacritty, i3 border colors, polybar colors, rofi, dunst, shell prompt, GTK, Ghostty.

---

## Tool differences vs niri-rice

| Feature | niri-rice (Wayland) | i3-rice (X11) |
|---------|--------------------|--------------------|
| Compositor | niri | i3-gaps + picom |
| Bar | waybar | polybar |
| App launcher | fuzzel | rofi |
| Notifications | mako | dunst |
| Screenshots | grim + slurp | maim + xdotool |
| Clipboard | wl-clipboard | xclip |
| Theme switcher | fuzzel --dmenu | rofi -dmenu |
| Clipboard daemon | wl-paste --watch | xclip polling loop |
| Lock screen | swaylock / hyprlock | i3lock |

---

## Files

```
i3-rice/
├── install.sh
├── README.md
├── config/
│   ├── i3/config              — i3-gaps config
│   ├── polybar/
│   │   ├── config             — polybar bar config
│   │   ├── colors.ini         — Nord defaults (rewritten by theme switch)
│   │   └── launch.sh          — simple launcher
│   ├── rofi/config.rasi       — Nord-themed rofi
│   ├── dunst/dunstrc          — Nord-themed dunst
│   └── picom/picom.conf       — rounded corners + shadows
└── bin/
    ├── alacritty-theme-switch — theme switcher (11 Nord-family themes)
    ├── i3-rice-polybar        — flock-guarded polybar launcher
    ├── i3-rice-lock           — i3lock with theme-aware bg color
    ├── i3-workspace-picker    — rofi workspace switcher
    ├── i3-rename-workspace    — rofi workspace rename
    ├── i3-quick-workspace     — jump to named workspace
    ├── rice-scratchpad-terminal — toggle tmux scratchpad (Python)
    ├── rice-screenshot-picker — maim screenshot with rofi picker
    ├── rice-clipboard-daemon  — xclip polling clipboard daemon
    ├── rice-clipboard-store   — append to ~/.local/state/niri-rice/clipboard.jsonl
    ├── rice-clipboard-picker  — rofi clipboard history picker (xclip)
    ├── polybar-system         — CPU/MEM/TEMP for polybar custom module
    └── waybar-system          — waybar JSON system module (shared)
```
