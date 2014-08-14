(setq ebib-preload-bib-files '("~/Documents/literature/bibs/bibs.bib"))
;; keywords
(setq ebib-keywords-file "~/Documents/literature/bibs/ebib-keywords.txt")
(setq ebib-keywords-use-only-file t)
(setq ebib-keywords-field-keep-sorted t)
;; layout
(setq ebib-layout 'custom)
(setq ebib-width 0.5)
;; (setq ebib-window-vertical-split t)
(setq ebib-allow-identical-fields t)
(setq ebib-file-associations '(("pdf" . "acroread") ("ps" . "gv")))


(provide 'init-ebib)