#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$profile_dir/.." && pwd)"
backup_dir="$HOME/.dotfiles-backup/niri-rice-$(date +%Y%m%d-%H%M%S)"
install_packages=0

usage() {
    cat <<'EOF'
Usage: ./niri-rice/install.sh [--install-packages]

Options:
  --install-packages  Install recommended packages with apt, dnf, or pacman before applying config.
  -h, --help          Show this help.
EOF
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --install-packages)
            install_packages=1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 1
            ;;
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
    local begin="# BEGIN niri-rice shell prompt"
    local end="# END niri-rice shell prompt"

    touch "$bashrc"
    if grep -qF "$begin" "$bashrc"; then
        return
    fi

    backup_path "$bashrc"
    cat >> "$bashrc" <<'EOF'

# BEGIN niri-rice shell prompt
if [ -f "$HOME/.config/shell-prompt/prompt.bash" ]; then
    . "$HOME/.config/shell-prompt/prompt.bash"
fi
# END niri-rice shell prompt
EOF
}

check_optional_commands() {
    local missing=()
    local command_name

    for command_name in niri waybar fuzzel mako makoctl alacritty tmux fish emacs; do
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

install_recommended_packages() {
    if command -v apt-get >/dev/null 2>&1; then
        local requested=(
            niri
            waybar
            fuzzel
            mako-notifier
            alacritty
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
            jq
            python3
            papirus-icon-theme
            fonts-jetbrains-mono
            fonts-font-awesome
        )
        local available=()
        local missing=()
        local package_name

        sudo apt-get update
        for package_name in "${requested[@]}"; do
            if apt-cache show "$package_name" >/dev/null 2>&1; then
                available+=("$package_name")
            else
                missing+=("$package_name")
            fi
        done

        if [ "${#available[@]}" -gt 0 ]; then
            sudo apt-get install -y "${available[@]}"
        fi
        if [ "${#missing[@]}" -gt 0 ]; then
            printf 'Packages not found in apt repositories: %s\n' "${missing[*]}" >&2
            printf 'Install those manually if your distro provides them through another source.\n' >&2
        fi
        return
    fi

    if command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y \
            niri waybar fuzzel mako alacritty foot tmux fish emacs \
            brightnessctl playerctl pavucontrol grim slurp wl-clipboard jq python3 \
            papirus-icon-theme jetbrains-mono-fonts fontawesome-fonts
        return
    fi

    if command -v pacman >/dev/null 2>&1; then
        sudo pacman -Syu --needed \
            niri waybar fuzzel mako alacritty foot tmux fish emacs \
            brightnessctl playerctl pavucontrol grim slurp wl-clipboard jq python \
            papirus-icon-theme ttf-jetbrains-mono ttf-font-awesome
        return
    fi

    echo "No supported package manager found. Install recommended packages manually." >&2
}

if [ "$install_packages" -eq 1 ]; then
    install_recommended_packages
fi

install_path "$profile_dir/config/alacritty" "$HOME/.config/alacritty"
install_path "$profile_dir/config/fuzzel" "$HOME/.config/fuzzel"
install_path "$profile_dir/config/ghostty" "$HOME/.config/ghostty"
install_path "$profile_dir/config/foot" "$HOME/.config/foot"
install_path "$profile_dir/config/mako" "$HOME/.config/mako"
install_path "$profile_dir/config/niri" "$HOME/.config/niri"
install_path "$profile_dir/config/shell-prompt" "$HOME/.config/shell-prompt"
install_path "$profile_dir/config/waybar" "$HOME/.config/waybar"
install_path "$profile_dir/config/fish/config.fish" "$HOME/.config/fish/config.fish"
install_path "$profile_dir/config/tmux/tmux.conf" "$HOME/.tmux.conf"
install_path "$repo_root/init.el" "$HOME/.emacs.d/init.el"

mkdir -p "$HOME/.local/bin"
for script in "$profile_dir"/bin/*; do
    install_path "$script" "$HOME/.local/bin/$(basename "$script")"
    chmod +x "$HOME/.local/bin/$(basename "$script")"
done

ensure_bash_prompt

"$HOME/.local/bin/alacritty-theme-switch" nord >/dev/null
configure_gtk_defaults
configure_copilot_cli

if command -v niri >/dev/null 2>&1; then
    niri validate --config "$HOME/.config/niri/config.kdl"
fi

check_optional_commands

echo "niri rice installed."
echo "Backups, if any, are in: $backup_dir"
echo "Restart niri or reload its config to apply compositor startup changes."
