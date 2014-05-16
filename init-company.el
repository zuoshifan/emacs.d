(add-hook 'after-init-hook 'global-company-mode)

;; does not matter, I never use this hotkey
(global-set-key (kbd "C-c o") 'company-complete)
(setq company-require-match nil)

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            )))

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-cmake)
     ;; I donot like the downcase code in company-dabbrev
     ;; More convinent to use hippie-expand (M-/)
     ;; (setq company-backends (delete 'company-dabbrev company-backends))
     (setq company-dabbrev-downcase nil)
     (setq company-show-numbers t)
     (setq company-backends (delete 'company-ropemacs company-backends)) ; can't work with TRAMP
     (setq company-backends (delete 'company-capf company-backends)) ;New version company-mode always include company-capf in the backends, see https://github.com/company-mode/company-mode/commit/66d8567a78a9eb86a5fd549fbbb1d90faf692a17 and https://github.com/company-mode/company-mode/issues/84
     (setq company-begin-commands '(self-insert-command))
     (setq company-idle-delay 0.2)
     ))


;;; Can't work with TRAM
;; (eval-after-load "python"
;;   '(progn
;;      ;; Initialize Pymacs
;;      (autoload 'pymacs-apply "pymacs")
;;      (autoload 'pymacs-call "pymacs")
;;      (autoload 'pymacs-eval "pymacs" nil t)
;;      (autoload 'pymacs-exec "pymacs" nil t)
;;      (autoload 'pymacs-load "pymacs" nil t)
;;      ;; (require 'pycomplete)  ; can only be used in python-mode.el, not python.el carried by emacs
;;      ;; (message "loading repomacs")

;;      ;; Initialize Rope
;;      (setq ropemacs-global-prefix "C-x /")
;;      (pymacs-load "ropemacs" "rope-")
;;      (setq ropemacs-enable-autoimport t)))


(provide 'init-company)
