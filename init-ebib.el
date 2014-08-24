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


(setq ebib-entry-types 
      '((article
         (author title journal year)
         (volume number pages month note eprint eid adsurl adsnote archivePrefix primaryClass))
        (book
         (author title publisher year)
         (editor volume number series address edition month note))
        (booklet
         (title)
         (author howpublished address month year note))
        (inbook
         (author title chapter pages publisher year)
         (editor volume series address edition month note))
        (incollection
         (author title booktitle publisher year)
         (editor volume number series type chapter pages address edition month note))
        (inproceedings
         (author title booktitle year)
         (editor pages organization publisher address month note))
        (manual
         (title)
         (author organization address edition month year note))
        (misc nil
              (title author howpublished month year note))
        (mastersthesis
         (author title school year)
         (address month note))
        (phdthesis
         (author title school year)
         (address month note))
        (proceedings
         (title year)
         (editor publisher organization address month note))
        (techreport
         (author title institution year)
         (type number address month note))
        (unpublished
         (author title note)
         (month year))))


(provide 'init-ebib)