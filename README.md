# dotfiles
Collection of dotfiles

## niri rice

The `niri-rice/` profile installs the themed niri desktop setup:

```bash
./niri-rice/install.sh --install-packages
```

> **Hyper-V / VM users:** niri requires DRM/KMS device access that Hyper-V
> does not expose — use `sway-rice` instead (see below).

## sway rice

The `sway-rice/` profile installs sway with the same look, keybindings,
and tooling as niri-rice.  Works on Hyper-V VMs and any machine where niri
cannot run, via wlroots Pixman software rendering:

```bash
./sway-rice/install.sh --install-packages
```

The installer registers a `Sway (Rice)` display-manager session that sets
`WLR_RENDERER=pixman` automatically for Hyper-V compatibility.  See
[`sway-rice/README.md`](sway-rice/README.md) for details.
