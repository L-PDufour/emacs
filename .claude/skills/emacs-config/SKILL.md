---
name: emacs-config
description: Ground rules for editing this Emacs configuration — it is a literate Org config on NixOS with an IGC-branch Emacs under Sway/Wayland, and on OpenBSD with the ports Emacs. Load this before changing README.org, init.el, early-init.el, or any use-package block, and before adding, removing or configuring a package.
---

# Editing this Emacs configuration

## The .el files are generated — never edit them

`init.el` and `early-init.el` are tangled from the `#+begin_src emacs-lisp`
blocks in `README.org`. Both carry the header *"Tangled from README.org — do
not edit by hand"*. The next tangle overwrites anything written directly into
them.

- Make every change as a source block in `README.org`.
- Which file a block feeds comes from the `:header-args:emacs-lisp:` property
  of its section — `:tangle early-init.el` on *Early Init*, `:tangle init.el`
  on *Init* and every top-level section after it. The global default on line 4
  is `:tangle no`.
- Only `emacs-lisp` blocks tangle. The `yaml`, `nix` and `authinfo` blocks are
  illustrations.
- Commit the regenerated `.el` files together with the `README.org` change.
  Both are tracked, and **the stale tangle is what actually boots.**

## Verify with `check_config` after every edit

Call the `check_config` tool. It re-tangles `README.org` in a scratch
directory, reports whether the checked-in `.el` files are still in sync, and
byte-compiles the result. Fix whatever it reports before saying you are done.

A change is not finished when the prose reads well. It is finished when
`check_config` comes back in sync and clean.

Warnings about free variables from packages that are not on the load-path of
the subprocess doing the compile are expected noise.

Indent new source blocks with **spaces, not tabs**. Org re-indents tab-indented
blocks differently across versions, which rewrites the whole file and buries
the real change in a whitespace diff.

## Packages: Nix under NixOS, package.el on OpenBSD

The bootstrap in `README.org` detects the package source once:

- `/etc/NIXOS` exists or `NIX_PROFILES` is set → Nix/home-manager provides
  every package. `use-package-always-ensure` and `package-archives` are nil.
- Otherwise (the OpenBSD laptop) → `package.el` installs from GNU, NonGNU and
  MELPA into `var/elpa`, and `use-package-always-ensure` is t.

A third-party `use-package` form therefore carries **no `:ensure` keyword**: it
inherits the mode. `:ensure nil` is reserved for built-ins (`emacs`, `org`,
`dired`, `eshell`, `gnus`, `tramp`, `xref`, `window`, `smerge-mode`, `project`,
`isearch`, `flymake`, `eldoc`, `comint`, `eglot`, `which-key`), sub-features
that are not separate packages (`ghostel-eshell`, `eca-chat`, …), the vendored
`templ-ts-mode`, and `gptel-preset-collection` (installed by `package-vc`).
Never add `:straight t`.

To add a package on NixOS: add the `epkgs.*` entry to the home-manager package
list — that change is the user's to make. On OpenBSD the form installs it on
the next start; run `M-x package-refresh-contents` first if the archive
metadata is stale.

`package-enable-at-startup` is nil in both modes and the `package-initialize`
call in `init.el` is **load-bearing**: it activates the Nix packages under
`site-lisp/elpa/` or the native ones under `var/elpa/` and loads their
autoloads. Do not remove it and do not enable `package-enable-at-startup`.

## `user-emacs-directory` is not the config root

`early-init.el` repoints it at `~/.emacs.d/var/`, so a bare
`user-emacs-directory` names the *state* directory. For a path inside the
config repository use `my-gptel-config-root` or
`(file-name-directory my/literate-config-file)`.

## Environment quirks that have already burned someone

- **IGC branch**: `igc-step-interval` is the only knob, and it is unbound on a
  stock build — always guard with `boundp`. `igc-step-multiplier` and
  `igc-cons-threshold` do not exist. `focus-out-hook` was removed in Emacs 28;
  use `after-focus-change-function`.
- **OpenBSD**: the ports Emacs is 31.1, built with tree-sitter but **without**
  native compilation. Guard those knobs with `featurep`/`boundp` as the config
  already does. `system-type` is `berkeley-unix`; libghostty does not exist
  there, so `ghostel` is skipped.
- **PGTK/Wayland**: GUI frames use the Wayland clipboard natively. The `xclip`
  shim is tty-only, via `tty-setup-hook`.
- `read-process-output-max` stays at 1 MiB. Larger values defeat themselves
  against the Linux `fs.pipe-max-size` cap — the `F_SETPIPE_SZ` fcntl fails
  silently and leaves the pipe at 64 kB.

## Do not reintroduce what was deliberately removed

Check `git log` before "restoring" anything that looks missing:

- `native-comp-deferred-compilation` (use `native-comp-jit-compilation`)
- `idle-update-delay` (removed in Emacs 30)
- `eglot-events-buffer-size` (obsolete since Eglot 1.16 — use
  `eglot-events-buffer-config`)
- **Evil / Meow** — no modal editing here, by choice
- **Flycheck** — diagnostics are Flymake's, under `C-c e`. Running both
  double-reports every error.

## LLM configuration

`gptel` has exactly one backend: **DeepSeek**. The key is read lazily by
`my-gptel-key` — `~/.authinfo.gpg`, then `DEEPSEEK_API_KEY`, then a prompt.
Never commit a key. Do not add a second backend or a proxy unless asked.

## Vendored code

`site-lisp/templ-ts-mode/` is third-party, not config. It is a submodule —
`.gitmodules` points at `L-PDufour/templ-ts-mode`, pinned to a commit on the
fork's `wip` branch. On a fresh clone run `git submodule update --init`; do not
edit files inside it.
