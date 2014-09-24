;; see https://linuxtoy.org/archives/emacs-eshell.html
;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat "" (user-login-name) "@" (system-name) " "
;;                 (eshell/pwd) "% ")))

(setq eshell-prompt-function
      (lambda ()
        (concat "" (user-login-name) "@" (system-name) ":" "$ ")))


(setq
 eshell-save-history-on-exit t
 eshell-history-size 512
 eshell-hist-ignoredups t
 eshell-cmpl-ignore-case t
 eshell-cp-interactive-query t
 eshell-ln-interactive-query t
 eshell-mv-interactive-query t
 eshell-rm-interactive-query t
 eshell-mv-overwrite-files nil
 ;; aliases-file 里面不能有多余的空行，否则会报正则表达式错误
 ;; eshell-aliases-file (expand-file-name "_eshell/eshell-alias" init-dir)

 eshell-highlight-prompt   t
 ;; 提示符设置，下面两项必须对应起来，
 ;; 否则会报 read-only，并且不能补全什么的
 ;; eshell-prompt-regexp      "^[^#$\n]* [#>]+ "
 ;; eshell-prompt-function    (lambda nil
 ;;                             (concat
 ;;                              (abbreviate-file-name
 ;;                               (eshell/pwd))
 ;;                              (if
 ;;                                  (=
 ;;                                   (user-uid)
 ;;                                   0)
 ;;                                  " # " " >>> ")))
 )

;; (defalias 'ff 'find-file)
;; (defalias 'ee (lambda()(find-file (expand-file-name "44eshell.el" init-dir))))
;; (defalias 'aa (lambda()(find-file eshell-aliases-file)))
;; (defalias 'rr (lambda()(find-file (expand-file-name "qref.org" sand-box))))
;; (defalias 'ss  'shell-command-to-string)


;; (defun eshell/ii (file)(ido-find-file file))
;; (defun eshell/ed (file1 file2)(ediff-files file1 file2))



;; 按一次 C-a 到命令在前面，再按一次到命令提示符的前面，感觉用处不大
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))