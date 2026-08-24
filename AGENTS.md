# AGENTS.md

Emacs configuration for **NixOS + the IGC branch + Sway (PGTK/Wayland)**.

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
   free variables from Nix-provided packages are expected noise.
3. Launching Emacs and checking startup for errors / unexpected modeline output.

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

It is recorded as a **gitlink** (`160000`) with no `.gitmodules` entry, so a
fresh `git clone` leaves the directory empty and nothing tells git where to
fetch it from. Nothing here depends on it at startup, so this is untidy rather
than broken — but either add a `.gitmodules` entry or commit the files
outright before relying on a clean clone.
