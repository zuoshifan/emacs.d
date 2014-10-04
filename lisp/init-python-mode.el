(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


;; indent offset
(setq python-indent-offset 4)

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(eval-after-load 'python
  '(require 'flymake-python-pyflakes))

;; jedi setup
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook '(lambda ()
                               (when *emacs24*
                                 (anaconda-mode)
                                 (eldoc-mode -1)
                                 ;; (add-to-list 'company-backends 'company-jedi)
                                 ;; (add-to-list 'company-backends 'company-anaconda)
                                 )
                               (flymake-python-pyflakes-load)))

;; See: http://stackoverflow.com/questions/15493342/have-emacs-edit-python-docstrings-using-rst-mode
;; Note that this will switch to rst-mode for every triple-quoted string, not just the ones at the start of a function definition. You could probably restrict it to just the ones at the start of a function definition with a more complex front regex, but I'm not completely sure how to handle it since I think mmm-mode definitions by default match a line at a time.
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (mmm-add-classes
;;  '((python-rst
;;     :submode rst-mode
;;     :front "^ *[ru]?\"\"\"[^\"]*$"
;;     ;; :front "^ *[ru]?\"\"\"[^\"]*"
;;     :back "^ *\"\"\""
;;     ;; :back "\"\"\"$"
;;     :include-front t
;;     :include-back t
;;     :end-not-begin t)))
;; (mmm-add-mode-ext-class 'python-mode nil 'python-rst)


(provide 'init-python-mode)
