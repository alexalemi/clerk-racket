#lang racket/base
;; Shared shell HTML/CSS so the live server and the static build both
;; render with identical styling.

(provide shell-css
         live-shell-html
         static-shell-html)

(define shell-css
  (string-append
   "body{font:14px/1.5 system-ui,sans-serif;max-width:920px;margin:2em auto;padding:0 1em;color:#111}"
   ".clerk-cell{border-left:3px solid #ddd;padding:.5em .8em;margin:.8em 0}"
   ".clerk-cell[data-cell-kind=expr]{border-left-color:#3a7}"
   ".clerk-cell[data-cell-kind=define]{border-left-color:#37a}"
   ".clerk-cell[data-cell-kind=meta]{border-left-color:#aaa}"
   "pre.clerk-source{background:#f6f6f6;padding:.4em .6em;margin:0 0 .3em;overflow:auto;font:13px/1.4 ui-monospace,monospace}"
   "pre.clerk-value{background:#fffae0;padding:.4em .6em;margin:0;overflow:auto;font:13px/1.4 ui-monospace,monospace}"
   "pre.clerk-error{background:#fde0e0;padding:.4em .6em;margin:0;color:#700;overflow:auto;font:13px/1.4 ui-monospace,monospace}"
   ".clerk-badge{float:right;font:10px ui-monospace,monospace;text-transform:uppercase;letter-spacing:.05em;padding:1px 5px;border-radius:2px;background:#eee;color:#555}"
   ".clerk-cell[data-status=fresh] .clerk-badge{background:#cfe8d0;color:#264}"
   ".clerk-cell[data-status=dep-dirty] .clerk-badge{background:#fde4b6;color:#653}"
   ".clerk-cell[data-status=cached] .clerk-badge{background:#eee;color:#888}"
   ".clerk-cell[data-status=error] .clerk-badge{background:#fcc;color:#700}"
   ".clerk-cell[data-status=fresh]{animation:clerk-flash 1s ease-out}"
   ".clerk-cell[data-status=dep-dirty]{animation:clerk-flash 1s ease-out}"
   "@keyframes clerk-flash{from{background:#fff8d0}to{background:transparent}}"
   ".clerk-md{border-left-color:transparent;padding-left:0}"
   ".clerk-md .clerk-md-body h1{font-size:1.5em;margin:.4em 0}"
   ".clerk-md .clerk-md-body h2{font-size:1.25em;margin:.4em 0}"
   ".clerk-md .clerk-md-body h3{font-size:1.1em;margin:.4em 0}"
   ".clerk-md code{background:#f1f1f1;padding:0 .25em;border-radius:2px;font:13px ui-monospace,monospace}"
   "table.clerk-table{border-collapse:collapse;margin:.3em 0;font:13px ui-monospace,monospace}"
   "table.clerk-table th,table.clerk-table td{border:1px solid #ccc;padding:.15em .45em;text-align:left}"
   "table.clerk-table th{background:#f6f6f6}"
   ".clerk-image{margin:.3em 0}"
   ".clerk-image img,.clerk-image svg{max-width:100%;display:block}"
   ".clerk-image svg{height:auto}"
   "#status{position:fixed;top:.5em;right:1em;font:12px ui-monospace,monospace;padding:.2em .5em;background:#eee;border-radius:3px}"
   "#status.live{background:#cfc}"
   "#status.dead{background:#fcc}"))

(define (live-shell-html title)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\">"
   "<title>" title "</title>"
   "<style>" shell-css "</style></head><body>"
   "<div id=\"status\">connecting…</div>"
   "<main id=\"cells\"></main>"
   "<script src=\"/client.js\"></script>"
   "</body></html>"))

(define (static-shell-html title body)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\">"
   "<title>" title "</title>"
   "<style>" shell-css "</style></head><body>"
   "<main id=\"cells\">" body "</main>"
   "</body></html>"))
