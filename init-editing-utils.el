;; Use evil-mode (strongly recommended) will make this file much smaller!

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file "~/.emacs.d/.bookmarks.el"
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 ;; no annoying beep on errors
 visible-bell t)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; shrink minibuffer window automatically when remove some lines of text from the minibuffer
(setq resize-mini-windows t)

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(transient-mark-mode t)

(define-key global-map (kbd "RET") 'newline-and-indent)

;;----------------------------------------------------------------------------
;; Zap *up* to char is a more sensible default
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;;----------------------------------------------------------------------------
;; use ispell-complete-word
;;----------------------------------------------------------------------------
;; (global-set-key (kbd "M-g i") 'ispell-complete-word)  ;; prefix "M-g" is used by init-thing-edit.el, which is loaded after this file
(global-set-key (kbd "M-n") 'ispell-complete-word)

;;----------------------------------------------------------------------------
;; transpose shortcuts
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t t") 'transpose-words)
(global-set-key (kbd "M-t M-t") 'transpose-words)
(global-set-key (kbd "M-t x") 'transpose-sexps)
(global-set-key (kbd "M-t s") 'transpose-sentences)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
;;;;;; (paren-activate)     ; activating mic-paren

;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(global-pointback-mode)

;;----------------------------------------------------------------------------
;; Turn on global-undo-tree-mode
;; Only C-x u invoke undo-tree-visualize, C-/ and C-_ invoke undo-tree-undo
;;----------------------------------------------------------------------------
(global-undo-tree-mode)

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(global-page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Use the printing package. This replaces the usual printing commands in the menu bar with a 'Pinting' submenu that contains various printing options.
;;----------------------------------------------------------------------------
(require 'printing)
(pr-update-menus)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down
;;----------------------------------------------------------------------------
(move-text-default-bindings)


;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;need install browse-kill-ring
(browse-kill-ring-default-keybindings)

(add-hook 'prog-mode-hook
          '(lambda ()
             ;; enable for all programming modes
             ;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
             (subword-mode)
             (if *emacs24* (electric-pair-mode 1))
             ;; eldoc, show API doc in minibuffer echo area
             (turn-on-eldoc-mode)
             ;; show trailing spaces in a programming mod
             (setq show-trailing-whitespace t)))


;; turns on auto-fill-mode, don't use text-mode-hook because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
;; (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'cc-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; {{ whitespace
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)
;; }}

;; some project prefer tab, so be it
;; @see http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(setq-default tab-width 4)
(defun toggle-indent-tab ()
  (interactive)
  (if indent-tabs-mode
      (progn
        (setq indent-tabs-mode nil)
        )
    (progn
        (setq indent-tabs-mode t)
        (setq indent-line-function 'insert-tab)
      )
      )
  )
(provide 'init-editing-utils)
