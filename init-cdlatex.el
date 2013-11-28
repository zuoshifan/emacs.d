;; CDLaTeX is a minor mode supporting fast insertion of environment
;; templates and math stuff in LaTeX.

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;; To turn CDLaTeX Minor Mode on and off in a particular buffer, use
;; `M-x cdlatex-mode'.

;; To turn on CDLaTeX Minor Mode for all LaTeX files
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

(provide 'init-cdlatex)