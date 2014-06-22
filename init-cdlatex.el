;; CDLaTeX is a minor mode supporting fast insertion of environment
;; templates and math stuff in LaTeX.

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;; To turn CDLaTeX Minor Mode on and off in a particular buffer, use
;; `M-x cdlatex-mode'.

;; To turn on CDLaTeX Minor Mode for all LaTeX files
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

;; when cdlatex-mode or org-cdlatex-mode are loaded, we need to change
;; the behaviour of yas/fallback to call cdlatex-tab
;; See http://spw.sdf.org/oldnotes/2010/12/making-yasnippet-and-cdlatex-mode-play-nice-together.html
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

(yas/advise-indent-function 'cdlatex-tab)
(yas/advise-indent-function 'org-cycle)
(yas/advise-indent-function 'org-try-cdlatex-tab)

(provide 'init-cdlatex)