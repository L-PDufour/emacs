# AGENTS.md

Emacs configuration for **NixOS + the IGC branch + Sway (PGTK/Wayland)**, and
for the same config on an **OpenBSD** laptop running the ports Emacs.

## Literate config — edit README.org, not the .el files

`init.el` and `early-init.el` are **references**: they are tangled (generated)
from the source blocks in `README.org` and both files carry the header
`Tangled from README.org — do not edit by hand`.

- Make all config changes as `#+begin_src emacs-lisp` blocks in `README.org`.
- Re-tangle after editing: saving `README.org` inside Emacs does it
  automatically (`my/org-babel-tangle-config`, *Literate Config* section);
  otherwise run `C-c C-v t` / `M-x org-babel-tangle`. Either regenerates
  both `early-init.el` and `init.el`.
- Commit the regenerated `.el` files together with the `README.org` change —
  both are tracked, and a stale tangle is what actually runs at startup.
- Which file a block feeds comes from the `:header-args:emacs-lisp:` property
  of its section: `:tangle early-init.el` on *Early Init*, `:tangle init.el`
  on *Init* and every top-level section after it. The global default on line 4
  is `:tangle no`, so a block tangles only where a section or the block itself
  overrides it.
- Only `emacs-lisp` blocks tangle. The `yaml`, `nix` and `authinfo` blocks in
  the prose are illustrations and never reach a `.el` file.
- Tangle with the same Emacs you run this config on. Older Org versions
  re-indent the tab-indented blocks differently and rewrite the whole file,
  burying the real change in a whitespace diff.
- Do not hand-edit `init.el` / `early-init.el` directly — the next tangle
  overwrites your changes.

## Packages: Nix on NixOS, package.el on OpenBSD

- The bootstrap in `README.org` detects the package source once:
  `/etc/NIXOS` or a non-empty `NIX_PROFILES` means **Nix/home-manager**
  provides every package; otherwise (the OpenBSD laptop) **package.el**
  installs from GNU, NonGNU and MELPA into `var/elpa`.
- A third-party `use-package` form carries **no `:ensure` keyword** — it
  inherits `use-package-always-ensure`, which is nil under Nix and t natively.
  `:ensure nil` is kept for built-ins (`emacs`, `org`, `dired`, `eshell`,
  `gnus`, `tramp`, `xref`, `window`, `smerge-mode`, `project`, `isearch`,
  `flymake`, `eldoc`, `comint`, `eglot`, `which-key`), sub-features that are
  not separate packages (`ghostel-eshell`, `ghostel-comint`,
  `ghostel-compile`, `eca-chat`), the vendored `templ-ts-mode`, and
  `gptel-preset-collection` (installed by `package-vc`). **Never** add
  `:straight t`.
- To add a package on NixOS, add the `epkgs.*` entry to the home-manager
  package list — that change is the user's to make. On OpenBSD the form
  installs it on the next start.
- `package-enable-at-startup` is nil in both modes; the call to
  `package-initialize` in `init.el` is **load-bearing** — it activates the
  Nix-provided packages under `site-lisp/elpa/` or the native ones under
  `var/elpa/`, and loads their autoloads. Drop it and autoloaded symbols like
  `global-corfu-mode` go void.
- `elpa/` and `var/*` are gitignored (runtime/cache state). Native packages
  live in `var/elpa`, so a checkout stays clean after packages install.

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
- **OpenBSD**: the ports Emacs is 31.1, built with tree-sitter but **without**
  native compilation. Guard those knobs with `featurep`/`boundp` as the config
  already does. `system-type` is `berkeley-unix`; libghostty does not exist
  there, so `ghostel` is skipped.
- **PGTK/Wayland**: GUI frames use the Wayland clipboard natively; the `xclip`
  shim is enabled only on tty frames via `tty-setup-hook`.
- `read-process-output-max` stays at 1 MiB — larger values defeat themselves
  against the Linux `fs.pipe-max-size` (1 MiB) cap; the `F_SETPIPE_SZ` fcntl
  fails silently and leaves the pipe at 64 kB.
- Do not reintroduce obsolete/dead code that has been deliberately removed:
  `native-comp-deferred-compilation` alias (use `native-comp-jit-compilation`),
  `idle-update-delay` (removed in Emacs 30), `eglot-events-buffer-size`
  (obsolete since Eglot 1.16 — use `eglot-events-buffer-config`), redundant
  `mode-line` cache hooks.
- Packages deliberately dropped, not merely unconfigured: **Evil**/Meow (no
  modal editing — plain Emacs bindings plus the repeat maps), and **Flycheck**
  (diagnostics are Flymake's, under `C-c e`; running both double-reports every
  error). Do not add them back.

## LLM config

`gptel` has exactly one backend: **DeepSeek** at `api.deepseek.com`. The key
is read lazily by `my-gptel-key` — `~/.authinfo.gpg`
(`machine api.deepseek.com login apikey password …`), then `DEEPSEEK_API_KEY`,
then a prompt. Never commit a key, and do not add a second backend or a proxy
without being asked for one.

## Verification

There is **no test/lint/build tooling** in this repo. Verify a change by:
1. Tangling from `README.org`, and checking `git diff` on the `.el` files
   shows only the region you meant to touch.
2. Byte-compiling the result (`emacs --batch -f batch-byte-compile init.el`)
   to catch read errors and malformed `use-package` forms. Warnings about
   free variables from packages absent on the subprocess load-path are
   expected noise.
3. Launching Emacs and checking startup for errors / unexpected modeline output.
   On OpenBSD, the first launch installs packages and may prompt once per
   tree-sitter grammar; check `*Warnings*` afterwards.

Steps 1 and 2 are also available as one call — `my-gptel-check-config`,
exposed to gptel as the `check_config` tool (*Agentic gptel* section). It
tangles into a scratch directory, diffs the result against the checked-in
`.el` files, and byte-compiles in a subprocess, touching nothing in the
working tree. Indent new source blocks with spaces, not tabs, so the tangle
is stable across Org versions.

## Agent definitions and skills

- `agents/config-reviewer.md` — a `gptel-agent` sub-agent that reviews a
  pending change to this config. Picked up via `gptel-agent-dirs`.
- `.claude/skills/emacs-config/SKILL.md` — these conventions as a loadable
  skill. `gptel-agent` builds its own system prompt and does **not** read this
  file, so anything an agent must know has to live in the skill as well. Keep
  the two in step.

## Vendored packages

`site-lisp/templ-ts-mode/` is a vendored standalone package (a separate git
repo) for the `templ` language. Treat it as third-party, not config.

It is a submodule: `.gitmodules` points at
`https://github.com/L-PDufour/templ-ts-mode.git`, and the gitlink is pinned to
a commit on that fork's `wip` branch. A fresh clone needs
`git submodule update --init` before the mode will load; do not edit files
inside it from this repo.
