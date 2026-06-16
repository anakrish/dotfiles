#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$profile_dir/.." && pwd)"
backup_dir="$HOME/.dotfiles-backup/niri-rice-$(date +%Y%m%d-%H%M%S)"
install_packages=0
build_niri=0
install_xwayland_satellite=0
niri_ref="main"
niri_source_dir="${XDG_CACHE_HOME:-$HOME/.cache}/niri-rice/niri"
xwayland_satellite_ref="v0.8.1"

usage() {
    cat <<'EOF'
Usage: ./niri-rice/install.sh [--install-packages] [--build-niri] [--niri-ref REF] [--xwayland-satellite]

Options:
  --install-packages    Install recommended packages with apt, dnf, or pacman before applying config.
  --build-niri          Install niri build dependencies, build niri from source, and install it under /usr/local.
  --niri-ref REF        Git branch, tag, or commit to build for --build-niri. Defaults to main.
  --xwayland-satellite  Install xwayland-satellite (rootless XWayland for X11 apps under niri). Uses a distro
                        package when available, otherwise builds it from source with cargo.
  -h, --help            Show this help.
EOF
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --install-packages)
            install_packages=1
            ;;
        --build-niri)
            build_niri=1
            ;;
        --xwayland-satellite)
            install_xwayland_satellite=1
            ;;
        --niri-ref)
            if [ "$#" -lt 2 ]; then
                echo "--niri-ref requires a branch, tag, or commit." >&2
                exit 1
            fi
            niri_ref="$2"
            shift
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

install_niri_build_dependencies() {
    if command -v apt-get >/dev/null 2>&1; then
        sudo apt-get update
        sudo apt-get install -y \
            git curl pkg-config build-essential gcc clang \
            libudev-dev libgbm-dev libxkbcommon-dev libegl1-mesa-dev \
            libwayland-dev libinput-dev libdbus-1-dev libsystemd-dev \
            libseat-dev libpipewire-0.3-dev libpango1.0-dev libdisplay-info-dev
        return
    fi

    if command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y \
            git curl pkg-config gcc clang \
            libudev-devel libgbm-devel libxkbcommon-devel wayland-devel \
            libinput-devel dbus-devel systemd-devel libseat-devel pipewire-devel \
            pango-devel cairo-gobject-devel libdisplay-info-devel
        return
    fi

    if command -v pacman >/dev/null 2>&1; then
        sudo pacman -Syu --needed \
            git curl pkgconf base-devel gcc clang \
            systemd mesa libxkbcommon wayland libinput dbus libseat pipewire \
            pango cairo libdisplay-info
        return
    fi

    echo "No supported package manager found. Install niri build dependencies manually." >&2
    exit 1
}

ensure_rust_toolchain() {
    if [ -f "$HOME/.cargo/env" ]; then
        # shellcheck disable=SC1091
        . "$HOME/.cargo/env"
    fi

    if ! command -v cargo >/dev/null 2>&1; then
        if ! command -v curl >/dev/null 2>&1; then
            echo "curl is required to install rustup." >&2
            exit 1
        fi

        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
        # shellcheck disable=SC1091
        . "$HOME/.cargo/env"
    fi

    if command -v rustup >/dev/null 2>&1; then
        rustup toolchain install stable
        rustup default stable
    fi

    if ! command -v cargo >/dev/null 2>&1; then
        echo "cargo was not found after installing Rust." >&2
        exit 1
    fi
}

checkout_niri_source() {
    mkdir -p "$(dirname "$niri_source_dir")"

    if [ -e "$niri_source_dir" ] && [ ! -d "$niri_source_dir/.git" ]; then
        echo "Refusing to overwrite non-git path: $niri_source_dir" >&2
        exit 1
    fi

    if [ ! -d "$niri_source_dir/.git" ]; then
        git clone https://github.com/niri-wm/niri.git "$niri_source_dir"
    fi

    git -C "$niri_source_dir" fetch --tags origin
    git -C "$niri_source_dir" checkout "$niri_ref"
    if [ "$(git -C "$niri_source_dir" symbolic-ref --short -q HEAD || true)" = "$niri_ref" ]; then
        git -C "$niri_source_dir" pull --ff-only origin "$niri_ref"
    fi
}

install_built_niri() {
    sudo install -Dm755 "$niri_source_dir/target/release/niri" /usr/local/bin/niri
    sudo install -Dm755 "$niri_source_dir/resources/niri-session" /usr/local/bin/niri-session
    sudo install -Dm644 "$niri_source_dir/resources/niri.desktop" /usr/local/share/wayland-sessions/niri.desktop
    sudo install -Dm644 "$niri_source_dir/resources/niri-portals.conf" /usr/local/share/xdg-desktop-portal/niri-portals.conf

    if [ -f "$niri_source_dir/resources/niri.service" ]; then
        sudo install -Dm644 "$niri_source_dir/resources/niri.service" /etc/systemd/user/niri.service
    fi
    if [ -f "$niri_source_dir/resources/niri-shutdown.target" ]; then
        sudo install -Dm644 "$niri_source_dir/resources/niri-shutdown.target" /etc/systemd/user/niri-shutdown.target
    fi

    if command -v systemctl >/dev/null 2>&1; then
        systemctl --user daemon-reload >/dev/null 2>&1 || true
    fi
}

build_and_install_niri() {
    install_niri_build_dependencies
    ensure_rust_toolchain
    checkout_niri_source
    cargo build --release --manifest-path "$niri_source_dir/Cargo.toml"
    install_built_niri
    /usr/local/bin/niri --version
}

install_xwayland_satellite_build_dependencies() {
    # xwayland-satellite needs clang to build and the Xwayland binary at runtime.
    if command -v apt-get >/dev/null 2>&1; then
        sudo apt-get update
        sudo apt-get install -y clang libclang-dev pkg-config xwayland
        return
    fi

    if command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y clang clang-devel pkg-config xorg-x11-server-Xwayland
        return
    fi

    if command -v pacman >/dev/null 2>&1; then
        sudo pacman -Syu --needed clang pkgconf xorg-xwayland
        return
    fi

    echo "No supported package manager found. Install xwayland-satellite build dependencies manually." >&2
    exit 1
}

build_and_install_xwayland_satellite() {
    # Prefer a distro package when one is available; otherwise build from source.
    if command -v apt-get >/dev/null 2>&1 && apt-cache show xwayland-satellite >/dev/null 2>&1; then
        sudo apt-get install -y xwayland-satellite
    elif command -v dnf >/dev/null 2>&1 && dnf -q list xwayland-satellite >/dev/null 2>&1; then
        sudo dnf install -y xwayland-satellite
    elif command -v pacman >/dev/null 2>&1 && pacman -Si xwayland-satellite >/dev/null 2>&1; then
        sudo pacman -Syu --needed xwayland-satellite
    else
        echo "Building xwayland-satellite $xwayland_satellite_ref from source..."
        install_xwayland_satellite_build_dependencies
        ensure_rust_toolchain
        cargo install --git https://github.com/Supreeeme/xwayland-satellite.git \
            --tag "$xwayland_satellite_ref" --locked xwayland-satellite

        # niri's startup PATH includes ~/.local/bin but not ~/.cargo/bin, so
        # expose the cargo-installed binary where niri can spawn it.
        mkdir -p "$HOME/.local/bin"
        ln -sf "$HOME/.cargo/bin/xwayland-satellite" "$HOME/.local/bin/xwayland-satellite"
    fi

    if command -v xwayland-satellite >/dev/null 2>&1; then
        xwayland-satellite --version || true
    elif [ -x "$HOME/.local/bin/xwayland-satellite" ]; then
        "$HOME/.local/bin/xwayland-satellite" --version || true
    fi
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
            xwayland
            xwayland-satellite
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
            papirus-icon-theme jetbrains-mono-fonts fontawesome-fonts \
            xorg-x11-server-Xwayland xwayland-satellite
        return
    fi

    if command -v pacman >/dev/null 2>&1; then
        sudo pacman -Syu --needed \
            niri waybar fuzzel mako alacritty foot tmux fish emacs \
            brightnessctl playerctl pavucontrol grim slurp wl-clipboard jq python \
            papirus-icon-theme ttf-jetbrains-mono ttf-font-awesome \
            xorg-xwayland xwayland-satellite
        return
    fi

    echo "No supported package manager found. Install recommended packages manually." >&2
}

if [ "$install_packages" -eq 1 ]; then
    install_recommended_packages
fi

if [ "$build_niri" -eq 1 ]; then
    build_and_install_niri
fi

if [ "$install_xwayland_satellite" -eq 1 ]; then
    build_and_install_xwayland_satellite
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
