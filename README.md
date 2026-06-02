# clerk-racket

Live, browser-rendered notebooks for Racket — inspired by
[Nextjournal's Clerk](https://clerk.vision) but for Racket.

A notebook is an **ordinary Racket file**. You write `#lang racket`
(or `#lang sicp`, etc.), comments become Markdown, and `raco clerk
serve` opens a browser tab that live-updates on every save. The file
still runs standalone with `racket file.rkt` — clerk just gives it a
richer view.

```racket
#lang racket
(require clerk)

;; # Pythagoras
;;
;; A quick demo. The text above is **Markdown** — written as
;; ordinary `;`-line comments.

(define (sq x) (* x x))
(define a 3)
(define b 4)

(sqrt (+ (sq a) (sq b)))
```

`raco clerk serve pythagoras.rkt` → browser shows the prose, the
defines, and the value `5`. Edit, save, watch it update.

## Install

```sh
git clone https://github.com/alexalemi/clerk-racket.git
raco pkg install --link ./clerk-racket
```

The `--link` install registers the directory as a development link, so
edits to clerk-racket's source take effect after `raco setup --pkgs
clerk-racket` without re-installing.

## Use

```sh
# Live server on a single file (default port 7777)
raco clerk serve mynote.rkt

# Directory mode: watch every .rkt under a tree. Saving any of them
# switches the live view to that file.
raco clerk serve ./notebooks

# Static HTML snapshot — no server, embeddable
raco clerk build mynote.rkt -o out.html
```

Notebooks can live anywhere on disk. `raco clerk` is registered
system-wide once the package is installed.

## How notebooks work

A clerk notebook is **plain Racket source**. The conventions are:

| Source                       | Renders as                                  |
|------------------------------|---------------------------------------------|
| `;` line comments (top level) | Markdown prose                             |
| `(define x ...)`             | Code cell + the bound value                 |
| `(any-expression)`           | Code cell + the value it produces           |
| `(require pict)` etc.        | Code cell, no value (silent)                |
| `(display ...)`, `(printf ...)` | Code cell + captured stdout shown inline |

Anything that can render to HTML via `file/convertible` renders
inline — picts as SVG, `2htdp/image` and `sicp-pict` outputs as PNG,
plots as SVG. Hashes and lists-of-hashes render as tables.

### Markdown supported

- Headers: `#`, `##`, `###`
- Paragraphs (blank line separates)
- **Bold** (`**text**`), *italic* (`*text*`), `code` (`` `text` ``)
- Unordered lists (`-`, `*`, `+`)
- Ordered lists (`1.`, `2.`, …)
- Links: `[text](url)` and bare URLs become clickable automatically
- LaTeX math: `$inline$` and `$$display$$` via KaTeX

### Typography

Prose cells render with book-like typography — serif body, sans
headings, generous heading margins, reading-width prose. Modeled on
[Sarabander's SICP edition](https://sarabander.github.io/sicp/). If
you have Linux Libertine / Biolinum installed locally (common on
Linux distros), clerk uses those; otherwise it falls back to Source
Serif 4 / Source Sans 3 from Google Fonts, then Georgia / system-ui.

Source blocks get **server-side syntax highlighting** via Racket's
own `syntax-color/racket-lexer` — the same lexer DrRacket uses to
color buffers — emitted as class-tagged spans with the
Google-Code-Prettify palette. No JS-side highlighter, no extra
dependencies.

### Captured output

`(display ...)`, `(printf ...)`, anything else that writes to
`current-output-port` during a cell's evaluation is captured and
shown in-cell, not dumped to your terminal. Writes to
`current-error-port` are captured separately and rendered with a
warning color. A cell that prints AND returns a value shows both
blocks.

### Directives

Three directives, written as `;; @clerk:NAME` immediately above a form:

```racket
;; @clerk:hide-code
(require pict (only-in 2htdp/image circle))

;; @clerk:hide-result
(define internal-thing (some-noisy-computation))

;; @clerk:viewer table
(for/list ([n (in-range 5)]) (hash 'n n 'sq (* n n)))
```

- `hide-code` — don't show the source of this cell
- `hide-result` — don't show the value
- `viewer NAME` — force a specific viewer (`image`, `table`, `default`)

### Custom viewers

```racket
(require clerk)

;; Register at module-load time; visible to the host renderer because
;; the viewer registry is shared across host and notebook namespace.
(register-viewer! 'biggify
  (lambda (v) (and (string? v) (= (string-length v) 1)))
  (lambda (s) (format "<div style=\"font-size:10em\">~a</div>" s)))

;; @clerk:viewer biggify
"!"
```

## Design

A few choices worth highlighting:

- **No custom `#lang`.** An earlier prototype used `#lang clerk
  <base>` as a meta-reader; we removed it because it broke
  standalone-runnability and forced hygiene gymnastics across
  restricted base langs (notably `#lang sicp`). Now any Racket file
  is potentially a clerk notebook — `raco clerk` decides whether to
  render it, the file itself doesn't declare anything special.

- **Per-cell error isolation.** Each cell is `eval`'d as a separate
  top-level form inside the notebook's module namespace, wrapped in
  `with-handlers`. A runtime error in cell 3 renders as an error
  block in cell 3 — cells 4, 5, 6 still produce their values. This
  matches Clerk's REPL-like model. The only thing that still blanks
  the whole notebook is a *read-time* error (unbalanced parens, etc.)
  — there's no way to break a notebook into cells if you can't read
  it.

- **Original-formatting preserved.** Source blocks display the
  literal characters from your file — multi-line argument lists,
  aligned `cond` clauses, custom indentation all survive unchanged.
  We stash the byte range of each form (`syntax-position` +
  `syntax-span`) at read time and slice the source string for
  rendering, instead of re-serializing the datum through
  `pretty-write`.

- **Absolute-state wire protocol.** Every save broadcasts a full
  snapshot of every cell via WebSocket; the browser does
  `replaceChildren`. We tried a diff-on-the-wire protocol but found
  the wire-bytes savings weren't worth the protocol complexity for
  notebook-sized payloads.

- **Robust file watching.** `filesystem-change-evt` (inotify) first,
  with mtime polling as fallback for filesystems where inotify is
  unreliable (notably encrypted home dirs / ecryptfs). Atomic-save
  races (the editor's brief unlink-then-rename window) are handled
  with a wait-for-existence loop before re-reading the file.
  Directory mode uses polling-only, since recursive inotify is
  noticeably more complex than periodic stat calls.

- **Shared module identity.** Rich values rely on
  `prop:convertible` from `file/convertible`. The host's
  `convertible?` predicate only matches values whose
  `prop:convertible` property points to the *same module instance*.
  We `namespace-attach-module` `file/convertible`, `pict`,
  `racket/draw`, `clerk`, and `clerk/viewer` from host to eval
  namespace so the property identities — and the viewer registry —
  line up.

## Status

This is v0.1. Works for `#lang racket` and `#lang sicp`. Other base
languages (`typed/racket`, `htdp/bsl`, etc.) should work in principle
but aren't tested yet. See `examples/` for working notebooks:

- `tour.rkt` — covers every viewer (picts, 2htdp/image, table, prose)
- `sicp-picture.rkt` — `#lang sicp` with the SICP picture language
- `sicp-ch1.rkt` — a chapter walkthrough with prose, exercises, and
  LaTeX-formatted math

## About this code

This was **vibecoded** — written through extended live conversation
with Claude (Anthropic), with me steering the design and Claude doing
most of the typing. The shape of the code reflects that: iterative,
heavily commented at the design-decision level, with explicit notes
about *why* something is the way it is rather than just *what* it does.
Treat the comments as a partial design journal as well as
documentation.

That doesn't make it bad code — but it's worth knowing if you're
auditing, extending, or copying patterns from it. There are places
where a careful manual rewrite would tighten things (e.g., the
markdown subset is hand-rolled and could be replaced with a real
parser).

## License

MIT.
