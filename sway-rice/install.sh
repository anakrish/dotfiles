#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$profile_dir/.." && pwd)"
backup_dir="$HOME/.dotfiles-backup/sway-rice-$(date +%Y%m%d-%H%M%S)"
install_packages=0

usage() {
    cat <<'EOF'
Usage: ./sway-rice/install.sh [--install-packages]

Options:
  --install-packages    Install recommended packages with apt, dnf, or pacman before applying config.
  -h, --help            Show this help.

Hyper-V / VM note:
  sway uses wlroots for rendering.  On Hyper-V (and other VMs without proper
  DRM device exposure) launch sway via the installed sway-session wrapper:
    WLR_RENDERER=pixman WLR_NO_HARDWARE_CURSORS=1 sway
  The installer registers a "Sway (Rice)" .desktop session that sets these
  variables automatically, so picking it from GDM/SDDM handles everything.
EOF
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --install-packages) install_packages=1 ;;
        -h|--help) usage; exit 0 ;;
        *) usage >&2; exit 1 ;;
    esac
    shift
done

backup_path() {
    local path="$1"
    if [ -e "$path" ] || [ -L "$path" ]; then
        mkdir -p "$backup_dir$(dirname "$path")"
        cp -a "$path" "$backup_dir$path"
    fi
}

install_path() {
    local source="$1"
    local target="$2"
    backup_path "$target"
    rm -rf "$target"
    mkdir -p "$(dirname "$target")"
    cp -a "$source" "$target"
}

ensure_bash_prompt() {
    local bashrc="$HOME/.bashrc"
    local begin="# BEGIN sway-rice shell prompt"
    local end="# END sway-rice shell prompt"
    touch "$bashrc"
    if grep -qF "$begin" "$bashrc"; then
        return
    fi
    backup_path "$bashrc"
    cat >> "$bashrc" <<'EOF'

# BEGIN sway-rice shell prompt
if [ -f "$HOME/.config/shell-prompt/prompt.bash" ]; then
    . "$HOME/.config/shell-prompt/prompt.bash"
fi
# END sway-rice shell prompt
EOF
}

configure_copilot_cli() {
    local settings_dir="$HOME/.copilot"
    local settings_file="$settings_dir/settings.json"
    mkdir -p "$settings_dir"
    if [ ! -f "$settings_file" ]; then
        printf '{}\n' > "$settings_file"
    fi
    python3 - "$settings_file" <<'PY'
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
try:
    settings = json.loads(path.read_text())
except json.JSONDecodeError:
    raise SystemExit(f"Refusing to update invalid JSON: {path}")
settings["theme"] = "dark"
settings["colorMode"] = "default"
path.write_text(json.dumps(settings, indent=2) + "\n")
PY
}

configure_gtk_defaults() {
    if ! command -v gsettings >/dev/null 2>&1; then
        return
    fi
    gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' >/dev/null 2>&1 || true
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark' >/dev/null 2>&1 || true
    gsettings set org.gnome.desktop.interface icon-theme 'Papirus-Dark' >/dev/null 2>&1 || true
}

install_session_desktop() {
    # Install a Wayland session .desktop for GDM/SDDM that enables Hyper-V
    # software rendering (WLR_RENDERER=pixman) before launching sway.
    local session_dir="/usr/local/share/wayland-sessions"
    local desktop_file="$session_dir/sway-rice.desktop"
    local wrapper="/usr/local/bin/sway-rice-session"

    if [ ! -d "$session_dir" ]; then
        sudo mkdir -p "$session_dir"
    fi

    sudo tee "$wrapper" > /dev/null <<'EOF'
#!/usr/bin/env bash
# sway-rice-session — Hyper-V / VM compatible sway session launcher.
# Sets WLR_RENDERER=pixman so sway falls back to software rendering when the
# VM exposes no proper DRM device.  Safe on real hardware (wlroots prefers
# hardware acceleration if available regardless of this env var).
export WLR_RENDERER=pixman
export WLR_NO_HARDWARE_CURSORS=1
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_TYPE=wayland
exec sway "$@"
EOF
    sudo chmod +x "$wrapper"

    sudo tee "$desktop_file" > /dev/null <<'EOF'
[Desktop Entry]
Name=Sway (Rice)
Comment=Sway tiling Wayland compositor — sway-rice profile, VM-compatible
Exec=sway-rice-session
Type=Application
DesktopNames=sway
EOF

    echo "Session installed: $desktop_file"
}

ensure_nerd_fonts() {
    local font_dir="$HOME/.local/share/fonts"
    local nerd_version="v3.4.0"
    local base_url="https://github.com/ryanoasis/nerd-fonts/releases/download/$nerd_version"
    local fonts=(
        "JetBrainsMono Nerd Font Mono:JetBrainsMono.zip"
        "Hack Nerd Font Mono:Hack.zip"
    )

    if ! command -v fc-list >/dev/null 2>&1; then
        echo "fontconfig (fc-list) not found; skipping Nerd Font installation." >&2
        return
    fi
    if ! command -v curl >/dev/null 2>&1 || ! command -v unzip >/dev/null 2>&1; then
        echo "curl and unzip are required to install Nerd Fonts; skipping." >&2
        return
    fi

    mkdir -p "$font_dir"
    local entry family zip_name tmp_dir installed_any=0
    for entry in "${fonts[@]}"; do
        family="${entry%%:*}"
        zip_name="${entry##*:}"
        if fc-list : family | grep -qiF "$family"; then
            continue
        fi
        echo "Installing $family from nerd-fonts $nerd_version..."
        tmp_dir="$(mktemp -d)"
        if curl --proto '=https' --tlsv1.2 -fsSL "$base_url/$zip_name" -o "$tmp_dir/$zip_name"; then
            unzip -qo "$tmp_dir/$zip_name" '*.ttf' -d "$font_dir" || \
                unzip -qo "$tmp_dir/$zip_name" -d "$font_dir"
            installed_any=1
        else
            echo "Failed to download $zip_name; install $family manually." >&2
        fi
        rm -rf "$tmp_dir"
    done

    if [ "$installed_any" -eq 1 ] && command -v fc-cache >/dev/null 2>&1; then
        fc-cache -f "$font_dir" >/dev/null 2>&1 || true
    fi
}

install_recommended_packages() {
    if command -v apt-get >/dev/null 2>&1; then
        local requested=(
            sway
            waybar
            fuzzel
            mako-notifier
            gnome-terminal
            foot
            tmux
            fish
            emacs
            brightnessctl
            playerctl
            pavucontrol
            grim
            slurp
            wl-clipboard
            swayidle
            swaylock
            xwayland
            jq
            curl
            unzip
            python3
            papirus-icon-theme
            fonts-jetbrains-mono
            fonts-font-awesome
        )
        local available=()
        local missing=()
        local pkg
        sudo apt-get update
        for pkg in "${requested[@]}"; do
            if apt-cache show "$pkg" >/dev/null 2>&1; then
                available+=("$pkg")
            else
                missing+=("$pkg")
            fi
        done
        if [ "${#available[@]}" -gt 0 ]; then
            sudo apt-get install -y "${available[@]}"
        fi
        if [ "${#missing[@]}" -gt 0 ]; then
            printf 'Packages not found in apt repositories: %s\n' "${missing[*]}" >&2
        fi
        return
    fi

    if command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y \
            sway waybar fuzzel mako gnome-terminal foot tmux fish emacs \
            brightnessctl playerctl pavucontrol grim slurp wl-clipboard \
            swayidle swaylock jq python3 curl unzip \
            papirus-icon-theme jetbrains-mono-fonts fontawesome-fonts \
            xorg-x11-server-Xwayland
        return
    fi

    if command -v pacman >/dev/null 2>&1; then
        sudo pacman -Syu --needed \
            sway waybar fuzzel mako gnome-terminal foot tmux fish emacs \
            brightnessctl playerctl pavucontrol grim slurp wl-clipboard \
            swayidle swaylock jq python curl unzip \
            papirus-icon-theme ttf-jetbrains-mono ttf-font-awesome \
            xorg-xwayland
        return
    fi

    echo "No supported package manager found. Install recommended packages manually." >&2
}

check_optional_commands() {
    local missing=()
    for cmd in sway waybar fuzzel mako makoctl gnome-terminal tmux fish emacs; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            missing+=("$cmd")
        fi
    done
    if [ "${#missing[@]}" -gt 0 ]; then
        printf 'Missing optional/recommended commands: %s\n' "${missing[*]}" >&2
    fi
}

# ── Run ───────────────────────────────────────────────────────────────────────

if [ "$install_packages" -eq 1 ]; then
    install_recommended_packages
fi

# sway-specific configs live in sway-rice/config/.
# Shared terminal/shell/tool configs are referenced from niri-rice/config/ to
# avoid duplication; both profiles ship with the full repo, so those paths
# always exist.
shared="$repo_root/niri-rice/config"

install_path "$profile_dir/config/sway"       "$HOME/.config/sway"
install_path "$profile_dir/config/waybar"     "$HOME/.config/waybar"
install_path "$shared/alacritty"              "$HOME/.config/alacritty"
install_path "$shared/foot"                   "$HOME/.config/foot"
install_path "$shared/fuzzel"                 "$HOME/.config/fuzzel"
install_path "$shared/ghostty"                "$HOME/.config/ghostty"
install_path "$shared/mako"                   "$HOME/.config/mako"
install_path "$shared/shell-prompt"           "$HOME/.config/shell-prompt"
install_path "$shared/fish/config.fish"       "$HOME/.config/fish/config.fish"
install_path "$shared/tmux/tmux.conf"         "$HOME/.tmux.conf"
install_path "$repo_root/init.el"             "$HOME/.emacs.d/init.el"

mkdir -p "$HOME/.local/bin"
for script in "$profile_dir"/bin/*; do
    install_path "$script" "$HOME/.local/bin/$(basename "$script")"
    chmod +x "$HOME/.local/bin/$(basename "$script")"
done

# Mask the distro waybar.service so sway's exec doesn't stack two bars.
if [ -e /usr/lib/systemd/user/waybar.service ] || [ -e /etc/systemd/user/waybar.service ]; then
    mkdir -p "$HOME/.config/systemd/user"
    ln -sfn /dev/null "$HOME/.config/systemd/user/waybar.service"
    systemctl --user disable --now waybar.service >/dev/null 2>&1 || true
fi

install_session_desktop

ensure_bash_prompt
ensure_nerd_fonts

"$HOME/.local/bin/alacritty-theme-switch" nord >/dev/null
configure_gtk_defaults
configure_copilot_cli

check_optional_commands

echo "sway-rice installed."
echo "Backups, if any, are in: $backup_dir"
echo "Log out and select 'Sway (Rice)' from your display manager, or run:"
echo "  WLR_RENDERER=pixman sway"
