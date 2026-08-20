# AGENTS.md

Emacs configuration for **NixOS + the IGC branch + Sway (PGTK/Wayland)**.

## Literate config — edit README.org, not the .el files

`init.el` and `early-init.el` are **references**: they are tangled (generated)
from the source blocks in `README.org` and both files carry the header
`Tangled from README.org — do not edit by hand`.

- Make all config changes as `#+begin_src emacs-lisp` blocks in `README.org`.
- After editing, re-tangle with `C-c C-v t` / `M-x org-babel-tangle`
  (`org-babel-tangle` on the executable conflict-resolver for source blocks,
  else it's `org-indent-mode`/`outline` keybinding). This regenerates both
  `early-init.el` and `init.el`.
- Header-args: `:tangle early-init.el` (line 18) and `:tangle init.el`
  (line 261) control which file each section feeds. The global property
  default is `:tangle no` (line 4), so a block only tangles where a
  section or block overrides it.
- Do not hand-edit `init.el` / `early-init.el` directly — the next tangle
  overwrites your changes.

## Packages come from Nix, not package.el

- All packages are provided by **Nix/home-manager** on the load-path. Every
  `use-package` uses `:ensure nil`. **Never** add `:ensure t` or `:straight t`.
- `package-archives` and `package-enable-at-startup` are nil; the call to
  `package-initialize` in `init.el` is **load-bearing** — it activates the
  Nix-provided packages under `site-lisp/elpa/` and loads their autoloads.
  Drop it and autoloaded symbols like `global-corfu-mode` go void.
- `elpa/` and `var/*` are gitignored (runtime/cache state).

## Runtime state lives under `var/`

`early-init.el` repoints `user-emacs-directory` to `~/.emacs.d/var/`, so all
generated state (autosave, recentf, saveplace, history, eln-cache, transient,
tramp, etc.) is kept out of the config root. Note this means a bare
`user-emacs-directory` reference resolves to `var/`.

## Environment quirks

- **IGC branch**: only `igc-step-interval` exists as a knob. Always guard with
  `boundp` (it's unbound on stock builds). `igc-step-multiplier`,
  `igc-cons-threshold` do NOT exist. `focus-out-hook` is gone (removed in 28);
  use `after-focus-change-function`.
- **PGTK/Wayland**: GUI frames use the Wayland clipboard natively; the `xclip`
  shim is enabled only on tty frames via `tty-setup-hook`.
- `read-process-output-max` stays at 1 MiB — larger values defeat themselves
  against the Linux `fs.pipe-max-size` (1 MiB) cap; the `F_SETPIPE_SZ` fcntl
  fails silently and leaves the pipe at 64 kB.
- Do not reintroduce obsolete/dead code that has been deliberately removed:
  `native-comp-deferred-compilation` alias (use `native-comp-jit-compilation`),
  `idle-update-delay` (removed in Emacs 30), redundant `mode-line` cache hooks.

## Verification

There is **no test/lint/build tooling** in this repo. Verify a change by:
1. Tangling from `README.org`.
2. Launching Emacs and checking startup for errors / unexpected modeline output.

## Vendored packages

`site-lisp/templ-ts-mode/` is a vendored standalone package (a separate git
repo) for the `templ` language. Treat it as third-party, not config.
