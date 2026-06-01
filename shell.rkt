#lang racket/base
;; Shared shell HTML/CSS so the live server and the static build both
;; render with identical styling.
;;
;; Design notes (cribbed from sarabander.github.io/sicp):
;;
;; - Two type registers: prose cells use a SERIF body with SANS-SERIF
;;   headings — book typography — while code cells stay in compact
;;   monospace utility chrome. The two registers signal "this is text
;;   you read" vs "this is text you evaluate."
;; - A warm palette (#f8f8f8 background, #383838 text, #804040 links)
;;   instead of stark black-on-white — easier on eyes and reads less
;;   like a terminal output dump.
;; - Reading-width prose: body caps at 760px and prose paragraphs
;;   further cap at 64ch so long sentences don't sprawl. Code blocks
;;   are free to use the full width.
;; - Generous heading margins so sections actually breathe.

(provide shell-css
         font-link-tags
         math-tags
         math-render-script
         live-shell-html
         static-shell-html)

;; Pre-connect to Google's font CDN and pull Source Serif 4 + Source
;; Sans 3 (designed to pair; closest widely-hosted match to Linux
;; Libertine / Biolinum). preconnect tags let the browser open the TLS
;; connection in parallel with HTML parsing.
(define font-link-tags
  (string-append
   "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
   "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
   "<link rel=\"stylesheet\" href=\""
   "https://fonts.googleapis.com/css2?"
   "family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&"
   "family=Source+Sans+3:wght@400;500&"
   "display=swap\">"))

;; KaTeX for math rendering. CSS is required for proper layout of
;; rendered math; the JS plus auto-render extension scans the DOM and
;; replaces `$...$` / `$$...$$` delimiters with rendered math. We pin
;; a version so the page is reproducible.
(define math-tags
  (string-append
   "<link rel=\"stylesheet\" href=\""
   "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css\">"
   "<script defer src=\""
   "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js\"></script>"
   "<script defer src=\""
   "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js\"></script>"))

;; Helper exposed on `window` that scans every prose cell for math
;; delimiters and renders them via KaTeX. Safe to call before KaTeX
;; loads (no-ops); the live client calls it on each save, and the
;; static shell calls it once on DOMContentLoaded.
(define math-render-script
  (string-append
   "<script>"
   "window.clerkRenderMath=function(root){"
   "if(typeof renderMathInElement!=='function')return;"
   "var els=(root||document).querySelectorAll('.clerk-md .clerk-md-body');"
   "els.forEach(function(el){renderMathInElement(el,{"
   "delimiters:["
   "{left:'$$',right:'$$',display:true},"
   "{left:'$',right:'$',display:false},"
   "{left:'\\\\[',right:'\\\\]',display:true},"
   "{left:'\\\\(',right:'\\\\)',display:false}"
   "],throwOnError:false});});"
   "};"
   ;; Render once on initial load (covers the static-build case and the
   ;; initial paint before the WS hands over).
   "document.addEventListener('DOMContentLoaded',function(){"
   "var poll=setInterval(function(){"
   "if(typeof renderMathInElement==='function'){"
   "clearInterval(poll);clerkRenderMath();"
   "}},50);"
   "});"
   "</script>"))

(define shell-css
  (string-append
   ;; Page chrome
   "html{background:#f8f8f8}"
   "body{font:15px/1.55 system-ui,-apple-system,Segoe UI,sans-serif;"
   "max-width:780px;margin:2.5em auto;padding:0 1.25em;"
   "color:#383838;background:#f8f8f8}"
   "a{color:#804040;text-decoration:none}"
   "a:hover{text-decoration:underline}"
   ;; Cell shell
   ".clerk-cell{border-left:3px solid #ddd;padding:.5em .8em;margin:1em 0}"
   ".clerk-cell[data-cell-kind=expr]{border-left-color:#3a7}"
   ".clerk-cell[data-cell-kind=define]{border-left-color:#37a}"
   ".clerk-cell[data-cell-kind=meta]{border-left-color:#bbb}"
   ;; Code blocks: no background — Sarabander-style. Visual separation
   ;; comes from the monospace font and the cell's left border. Only
   ;; error blocks get tinted, since they need to draw the eye.
   "pre.clerk-source{background:transparent;color:#383838;"
   "padding:.2em 0;margin:0 0 .2em;overflow:auto;"
   "font:13px/1.5 'JetBrains Mono','Fira Code','Inconsolata',"
   "'DejaVu Sans Mono',ui-monospace,monospace}"
   "pre.clerk-value{background:transparent;color:#383838;"
   "padding:.1em 0 .1em 1em;margin:0;overflow:auto;"
   "border-left:2px solid #d0c8b8;"  ; subtle bar to mark "this is the result"
   "font:13px/1.5 'JetBrains Mono','Fira Code','Inconsolata',"
   "'DejaVu Sans Mono',ui-monospace,monospace}"
   "pre.clerk-error{background:#fbe3e0;color:#6a1f1f;"
   "padding:.5em .7em;margin:0;overflow:auto;"
   "font:13px/1.5 'JetBrains Mono','Fira Code','Inconsolata',"
   "'DejaVu Sans Mono',ui-monospace,monospace;border-radius:2px}"
   ;; Syntax highlighting (Sarabander/Google-Code-Prettify palette).
   ;; Applied by spans emitted from highlight.rkt; targets pre.clerk-source.
   "pre.clerk-source .pln{color:#606060}"        ; plain (identifiers)
   "pre.clerk-source .kwd{color:#4070a0}"        ; keywords (define, if, …)
   "pre.clerk-source .lit{color:#509040}"        ; literals (numbers, #t/#f)
   "pre.clerk-source .str{color:#985098}"        ; strings
   "pre.clerk-source .com{color:#b08050;font-style:italic}"  ; comments
   "pre.clerk-source .opn,pre.clerk-source .clo{color:#989898}"  ; parens
   "pre.clerk-source .pun{color:#909020}"        ; operators / non-paren punctuation
   "pre.clerk-source .atn{color:#606}"           ; #:keyword arg labels
   "pre.clerk-source .err{color:#c06050}"        ; lex errors
   ;; Errored cells get a red left border so failures stand out without
   ;; needing a badge label. The clerk-error <pre> in the value block
   ;; carries the actual error text.
   ".clerk-cell[data-status=error]{border-left-color:#c06050}"
   ;; Prose cells: book-like type register
   ".clerk-md{border-left-color:transparent;padding:.2em 0}"
   ".clerk-md .clerk-md-body{"
   ;; Linux Libertine O first — the true Sarabander serif, ships with
   ;; most Linux distros. Source Serif 4 (Google) as web-font fallback,
   ;; then Georgia for any system that has neither.
   "font-family:'Linux Libertine O','Libertinus Serif',"
   "'Source Serif 4','Source Serif Pro',"
   "Georgia,'Times New Roman',serif;"
   "font-size:1.15em;line-height:1.55;color:#383838;"
   "max-width:64ch}"
   ;; Justified prose with browser hyphenation — book-like density
   ;; without the river-of-whitespace that justified text-without-hyphens
   ;; produces. Skip on short blocks (lists are display:block but their
   ;; <li>s shouldn't justify).
   ".clerk-md .clerk-md-body p{margin:.6em 0;"
   "text-align:justify;hyphens:auto;-webkit-hyphens:auto}"
   ".clerk-md .clerk-md-body h1,"
   ".clerk-md .clerk-md-body h2,"
   ".clerk-md .clerk-md-body h3{"
   ;; Linux Biolinum O first — the real Sarabander sans. Source Sans 3
   ;; (Google) as web-font fallback for systems without it.
   "font-family:'Linux Biolinum O','Libertinus Sans',"
   "'Source Sans 3','Source Sans Pro',"
   "system-ui,-apple-system,Segoe UI,sans-serif;"
   "font-weight:normal;color:#2a2a2a;line-height:1.2}"
   ".clerk-md .clerk-md-body h1{font-size:1.9em;margin:1.5em 0 .4em}"
   ".clerk-md .clerk-md-body h2{font-size:1.45em;margin:1.4em 0 .3em}"
   ".clerk-md .clerk-md-body h3{font-size:1.2em;margin:1.2em 0 .3em}"
   ".clerk-md .clerk-md-body h1:first-child,"
   ".clerk-md .clerk-md-body h2:first-child,"
   ".clerk-md .clerk-md-body h3:first-child{margin-top:.2em}"
   ;; Inline code in prose: no background; rely on the monospace font.
   ".clerk-md code{"
   "color:#5a3a2a;"
   "font:.9em 'JetBrains Mono','Fira Code','Inconsolata',"
   "'DejaVu Sans Mono',ui-monospace,monospace}"
   ".clerk-md .clerk-md-body ul,"
   ".clerk-md .clerk-md-body ol{margin:.5em 0 .5em 1.5em;padding:0}"
   ".clerk-md .clerk-md-body li{margin:.15em 0}"
   ".clerk-md .clerk-md-body strong{font-weight:600;color:#2a2a2a}"
   ".clerk-md .clerk-md-body em{color:#3a3a3a}"
   ;; Tables
   "table.clerk-table{border-collapse:collapse;margin:.4em 0;"
   "font:13px/1.4 ui-monospace,monospace}"
   "table.clerk-table th,table.clerk-table td{"
   "border:1px solid #d0c8b8;padding:.2em .55em;text-align:left}"
   "table.clerk-table th{background:#efe9d8;font-weight:normal;color:#5a4a2a}"
   ;; Inline images
   ".clerk-image{margin:.4em 0}"
   ".clerk-image img,.clerk-image svg{max-width:100%;display:block}"
   ".clerk-image svg{height:auto}"
   ;; Live status indicator
   "#status{position:fixed;top:.5em;right:1em;"
   "font:11px/1 ui-monospace,monospace;padding:.25em .55em;"
   "background:#eae5d8;color:#7a6a4a;border-radius:3px}"
   "#status.live{background:#d6e6d6;color:#2c4a2c}"
   "#status.dead{background:#f0c8c0;color:#6a1f1f}"))

(define (live-shell-html title)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\">"
   "<title>" title "</title>"
   font-link-tags
   math-tags
   "<style>" shell-css "</style>"
   math-render-script
   "</head><body>"
   "<div id=\"status\">connecting…</div>"
   "<main id=\"cells\"></main>"
   "<script src=\"/client.js\"></script>"
   "</body></html>"))

(define (static-shell-html title body)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\">"
   "<title>" title "</title>"
   font-link-tags
   math-tags
   "<style>" shell-css "</style>"
   math-render-script
   "</head><body>"
   "<main id=\"cells\">" body "</main>"
   "</body></html>"))
