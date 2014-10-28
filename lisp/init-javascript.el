(require-package 'json-mode)
(maybe-require-package 'js2-mode)
;; (maybe-require-package 'ac-js2)
(maybe-require-package 'coffee-mode)
(require-package 'js-comint)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

;; looks nodejs is more popular, if you prefer rhino, change to "js"
(setq inferior-js-program-command "node --interactive")

(require 'js-comint)
;; if use node.js, we need nice output
(setenv "NODE_NO_READLINE" "1")

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2")))

  (setq-default
   js2-basic-offset preferred-javascript-indent-level
   js2-bounce-indent-p nil)

  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;; Javascript nests {} and () a lot, so I find this helpful

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))



;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "js")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))

;; json
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist))

;; {{ js2-mode or javascript-mode
(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
      js2-bounce-indent-p t)

;; js-mode imenu enhancement
;; @see http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
(defun mo-js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                               (nil " \\([^ ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

(defun flymake-jshint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (arglist (list local-file)))
      (list "jshint" arglist)))

(defun mo-js-mode-hook ()
  (setq imenu-create-index-function 'mo-js-imenu-make-index)
  (flymake-mode 1)
  (setq flymake-err-line-patterns
        (cons '(".*: line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
                nil 1 2 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jshint-init)
               '("\\.json\\'" flymake-jshint-init))
  (flymake-mode 1)
  )
(add-hook 'js-mode-hook 'mo-js-mode-hook)

(cond
 ((and (>= emacs-major-version 24) (>= emacs-minor-version 1) (not *no-memory*))
  (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js2-mode) auto-mode-alist))
  (autoload 'js2-mode "js2-mode" nil t)
  (add-hook 'js2-mode-hook '(lambda ()
                              (js2-imenu-extras-mode)
                              (setq mode-name "JS2")
                              (require 'js-doc)
                              (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
                              (define-key js2-mode-map "@" 'js-doc-insert-tag)
                              ))
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  )
 (t
  (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js-mode) auto-mode-alist))
  ))
;; }}

(if *emacs24* (add-hook 'coffee-mode-hook 'flymake-coffee-load))

;; {{ js-beautify
(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org.
sudo pip install jsbeautifier"
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat "js-beautify"
                                     " --stdin "
                                     " --jslint-happy --brace-style=end-expand --keep-array-indentation "
                                     (format " --indent-size=%d " js2-basic-offset))
                             nil t)
    (goto-char orig-point)))
;; }}

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

(provide 'init-javascript)
