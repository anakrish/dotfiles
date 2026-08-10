#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$profile_dir/.." && pwd)"
backup_dir="$HOME/.dotfiles-backup/i3-rice-$(date +%Y%m%d-%H%M%S)"
install_packages=0

usage() {
    cat <<'EOF'
Usage: ./i3-rice/install.sh [--install-packages]

Options:
  --install-packages    Install recommended packages with apt before applying config.
  -h, --help            Show this help.
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
    local begin="# BEGIN i3-rice shell prompt"
    local end="# END i3-rice shell prompt"

    touch "$bashrc"
    if grep -qF "$begin" "$bashrc"; then
        return
    fi

    backup_path "$bashrc"
    cat >> "$bashrc" <<'EOF'

# BEGIN i3-rice shell prompt
if [ -f "$HOME/.config/shell-prompt/prompt.bash" ]; then
    . "$HOME/.config/shell-prompt/prompt.bash"
fi
# END i3-rice shell prompt
EOF
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
        curl -fsSL "$base_url/$zip_name" -o "$tmp_dir/$zip_name"
        unzip -q "$tmp_dir/$zip_name" -d "$tmp_dir/fonts"
        find "$tmp_dir/fonts" -name '*.ttf' -o -name '*.otf' | while read -r font_file; do
            cp "$font_file" "$font_dir/"
        done
        rm -rf "$tmp_dir"
        installed_any=1
    done

    if [ "$installed_any" -eq 1 ] && command -v fc-cache >/dev/null 2>&1; then
        fc-cache -f "$font_dir"
    fi
}

check_optional_commands() {
    local missing=()
    local command_name

    for command_name in i3 polybar rofi dunst picom gnome-terminal tmux fish emacs \
            brightnessctl playerctl pavucontrol maim xdotool xclip i3lock hsetroot; do
        if ! command -v "$command_name" >/dev/null 2>&1; then
            missing+=("$command_name")
        fi
    done

    if [ "${#missing[@]}" -gt 0 ]; then
        printf 'Missing optional/recommended commands: %s\n' "${missing[*]}" >&2
    fi
}

configure_copilot_cli() {
    local settings_dir="$HOME/.copilot"
    local settings_file="$settings_dir/settings.json"

    mkdir -p "$settings_dir"
    if [ ! -f "$settings_file" ]; then
        printf '{}\n' > "$settings_file"
    fi

    python3 - "$settings_file" <<'PY'
import json
import pathlib
import sys

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

configure_default_xsession() {
    local xsession="$HOME/.xsession"
    backup_path "$xsession"
    cat > "$xsession" <<'EOF'
#!/usr/bin/env bash
export XDG_CURRENT_DESKTOP=i3
export XDG_SESSION_TYPE=x11
exec "$HOME/.local/bin/i3-rice-session"
EOF
    chmod +x "$xsession"
}

install_recommended_packages() {
    if command -v apt-get >/dev/null 2>&1; then
        local requested=(
            i3
            i3-gaps
            polybar
            rofi
            dunst
            picom
            gnome-terminal
            foot
            tmux
            fish
            emacs
            brightnessctl
            playerctl
            pavucontrol
            maim
            xdotool
            xclip
            xautolock
            i3lock
            hsetroot
            jq
            curl
            unzip
            python3
            papirus-icon-theme
            fonts-jetbrains-mono
            fonts-font-awesome
        )
        local available=()
        local missing_pkgs=()
        local package_name

        sudo apt-get update
        for package_name in "${requested[@]}"; do
            if apt-cache show "$package_name" >/dev/null 2>&1; then
                available+=("$package_name")
            else
                missing_pkgs+=("$package_name")
            fi
        done

        if [ "${#available[@]}" -gt 0 ]; then
            sudo apt-get install -y "${available[@]}"
        fi
        if [ "${#missing_pkgs[@]}" -gt 0 ]; then
            printf 'Packages not found in apt repositories: %s\n' "${missing_pkgs[*]}" >&2
            printf 'Install those manually if your distro provides them through another source.\n' >&2
        fi
        return
    fi

    echo "No supported package manager found. Install recommended packages manually." >&2
}

if [ "$install_packages" -eq 1 ]; then
    install_recommended_packages
fi

# ── Install shared configs from niri-rice ─────────────────────────────────────
niri_rice="$repo_root/niri-rice"
if [ -d "$niri_rice/config/alacritty" ]; then
    install_path "$niri_rice/config/alacritty" "$HOME/.config/alacritty"
fi
if [ -d "$niri_rice/config/foot" ]; then
    install_path "$niri_rice/config/foot" "$HOME/.config/foot"
fi
if [ -d "$niri_rice/config/shell-prompt" ]; then
    install_path "$niri_rice/config/shell-prompt" "$HOME/.config/shell-prompt"
fi
if [ -f "$niri_rice/config/fish/config.fish" ]; then
    install_path "$niri_rice/config/fish/config.fish" "$HOME/.config/fish/config.fish"
fi
if [ -f "$niri_rice/config/tmux/tmux.conf" ]; then
    install_path "$niri_rice/config/tmux/tmux.conf" "$HOME/.tmux.conf"
fi
if [ -f "$repo_root/init.el" ]; then
    install_path "$repo_root/init.el" "$HOME/.emacs.d/init.el"
fi

# ── Install i3-rice specific configs ──────────────────────────────────────────
install_path "$profile_dir/config/i3"     "$HOME/.config/i3"
install_path "$profile_dir/config/polybar" "$HOME/.config/polybar"
install_path "$profile_dir/config/rofi"   "$HOME/.config/rofi"
install_path "$profile_dir/config/dunst"  "$HOME/.config/dunst"
install_path "$profile_dir/config/picom"  "$HOME/.config/picom"

# ── Install bin scripts ────────────────────────────────────────────────────────
mkdir -p "$HOME/.local/bin"
for script in "$profile_dir"/bin/*; do
    install_path "$script" "$HOME/.local/bin/$(basename "$script")"
    chmod +x "$HOME/.local/bin/$(basename "$script")"
done

# Make polybar launch.sh executable
chmod +x "$HOME/.config/polybar/launch.sh" 2>/dev/null || true

ensure_bash_prompt
ensure_nerd_fonts
configure_gtk_defaults
configure_copilot_cli
configure_default_xsession

"$HOME/.local/bin/alacritty-theme-switch" nord >/dev/null || true

check_optional_commands

echo "i3 rice installed."
echo "Backups, if any, are in: $backup_dir"
echo "Default xrdp session set to i3 via ~/.xsession."
echo "Log out/reconnect, or run: i3-msg restart"
