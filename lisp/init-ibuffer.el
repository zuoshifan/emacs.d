;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

(require-package 'fullframe)
(after-load 'ibuffer
 (fullframe ibuffer ibuffer-quit))

(require-package 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)


;; (eval-after-load 'ibuffer
;;   '(progn
;;      ;; Use human readable Size column instead of original one
;;      (define-ibuffer-column size-h
;;        (:name "Size" :inline t)
;;        (cond
;;         ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
;;         ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
;;         (t (format "%8d" (buffer-size)))))
;;      ;; Explicitly require ibuffer-vc to get its column definitions, which
;;      ;; can't be autoloaded
;;      (require 'ibuffer-vc)

;;      (setq ibuffer-expert t
;;            ibuffer-show-empty-filter-groups nil
;;            ibuffer-display-summary nil)

;;      (setq ibuffer-saved-filter-groups
;;            (quote (("default"
;;                     ("code" (or (mode . emacs-lisp-mode)
;;                                 (mode . cperl-mode)
;;                                 (mode . c-mode)
;;                                 (mode . java-mode)
;;                                 (mode . idl-mode)
;;                                 (mode . web-mode)
;;                                 (mode . lisp-mode)
;;                                 (mode . js2-mode)
;;                                 (mode . c++-mode)
;;                                 (mode . lua-mode)
;;                                 (mode . cmake-mode)
;;                                 (mode . ruby-mode)
;;                                 (mode . scss-mode)
;;                                 (mode . css-mode)
;;                                 (mode . csharp-mode)
;;                                 (mode . objc-mode)
;;                                 (mode . sql-mode)
;;                                 (mode . python-mode)
;;                                 (mode . coffee-mode)
;;                                 (mode . php-mode)
;;                                 (mode . sh-mode)
;;                                 (mode . json-mode)
;;                                 (mode . scala-mode)
;;                                 (mode . go-mode)
;;                                 (mode . erlang-mode)
;;                                 ))
;;                     ("dired" (or (mode . dired-mode)
;;                                  (mode . sr-mode)
;;                                  ))
;;                     ("erc" (mode . erc-mode))
;;                     ("planner" (or
;;                                 (name . "^\\*Calendar\\*$")
;;                                 (name . "^diary$")
;;                                 (mode . muse-mode)
;;                                 (mode . org-mode)
;;                                 (mode . org-agenda-mode)
;;                                 ))
;;                     ("emacs" (or
;;                               (name . "^\\*scratch\\*$")
;;                               (name . "^\\*Messages\\*$")))
;;                     ("gnus" (or
;;                              (mode . message-mode)
;;                              (mode . bbdb-mode)
;;                              (mode . mail-mode)
;;                              (mode . gnus-group-mode)
;;                              (mode . gnus-summary-mode)
;;                              (mode . gnus-article-mode)
;;                              (name . "^\\.bbdb$")
;;                              (name . "^\\.newsrc-dribble")))))))
;;      (add-hook 'ibuffer-mode-hook (lambda ()
;;                                     (ibuffer-vc-set-filter-groups-by-vc-root)
;;                                     (unless (eq ibuffer-sorting-mode 'filename/process)
;;                                       (ibuffer-do-sort-by-filename/process))
;;                                     (ibuffer-switch-to-saved-filter-groups "default")
;;                                     ))
;;      ))

(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))


;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer
  (require 'ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
