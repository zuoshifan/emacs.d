;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time
;; (setq emacs-load-start-time (current-time))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *macbook-pro-support-enabled* t)
(defconst *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(defconst *win32* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin))
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defconst *linux-x* (and window-system *linux*))
(defconst *xemacs* (featurep 'xemacs) )
(defconst *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))))
(defconst *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))))
(defconst *no-memory* (cond
                       (*is-a-mac*
                        (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                       (*linux* nil)
                       (t nil)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; Functions (load all files in defuns-dir)
; Copied from https://github.com/magnars/.emacs.d/blob/master/init.el
;----------------------------------------------------------------------------
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
      (load file)))


(require 'init-coding-system)
(require 'init-modeline)

;;;;;; (require 'cl-lib) ;; I already use emacs version 24.3, I don't need forward compatibility provided by `cl-lib'
;; (require 'init-compat) ;; idle require
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
(condition-case nil
    (when *win32*
      (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
      (require 'setup-cygwin)
      ;; better to set HOME env in GUI
      ;; (setenv "HOME" "c:/cygwin/home/someuser")
      )
  (error
   (message "setup-cygwin failed, continue anyway")))

(require 'idle-require)

(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

(require 'init-frame-hooks)
;; any file use flyspell should be initialized after init-spelling.el
;; actually, I don't know which major-mode use flyspell.
(when *spell-check-support-enabled*
  (require 'init-spelling))

;; (require 'init-xterm) ;; idle require
;; (require 'init-themes) ; color-themes 6.6.1 has some problem
;; (require 'init-osx-keys) ;; I'm not using mac OS X
(require 'init-gui-frames)
(require 'init-proxies)
(require 'init-maxframe)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
;; (require 'init-flymake)

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
;; (require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)

(require 'init-editing-utils)

(require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-smex)
(require 'init-helm)

;(require 'init-growl)
(require 'init-compile)
(require 'init-crontab)
;; (require 'init-textile) ;; idle require
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
;; (require 'init-javascript) ;; idle require
;; (require 'init-php)
(require 'init-org)
(require 'init-org-mime)
(require 'init-nxml)
;; (require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
;; (require 'init-haskell) ;; idle require
;; (require 'init-ruby-mode) ;; idle require
;; (require 'init-rails) ;; idle require
;; (require 'init-sql) ;; idle require
(require 'init-elisp)
(require 'init-marmalade)


;; (require 'init-org2blog) ;; idle require
(require 'init-yasnippet)
;; Use bookmark instead
(require 'init-zencoding-mode)
(require 'init-cc-mode)
(require 'init-gud)
(require 'init-cmake-mode)
;; (require 'init-csharp-mode) ;; idle require
(require 'init-linum-mode)
;(require 'init-delicious) ;make startup slow, I don't use delicious in w3m
(require 'init-emacs-w3m)
(require 'init-thing-edit)
(require 'init-which-func)
(require 'init-move-window-buffer)
;; (require 'init-gist)
(require 'init-moz)
(require 'init-gtags)
;; use evil mode (vi key binding)
(require 'init-evil)
(require 'init-sh)
(require 'init-ctags)
(require 'init-ace-jump-mode)
(require 'init-sunrise-commander)
(require 'init-bbdb)
;; (require 'init-gnus)
;; (require 'init-weibo) ;; idle require
;; (require 'init-lua-mode) ;; idle require
;;;;;; (require 'init-workgroups2)
(require 'init-term-mode)
(require 'init-web-mode)
(require 'init-sr-speedbar)
(require 'init-smartparens)

(require 'init-paredit)
(require 'init-lisp)
;; (require 'init-slime) ;; idle require
;; (require 'init-clojure)
;; (when (>= emacs-major-version 24)
;;   (require 'init-clojure-cider))
(require 'init-common-lisp)

(when *emacs24*
    (require 'init-company)
  ;; Choose either auto-complete or company-mode by commenting one of below two lines!
  ;; (require 'init-auto-complete) ; after init-yasnippeta to override TAB
  )
(require 'init-stripe-buffer)
(require 'init-eim) ;;  cannot be idle-required
(require 'init-tramp)
(require 'init-full-screen)
(require 'init-chinese-calendar)
(require 'init-eimp)
(require 'init-screenshot)
(require 'init-epa)
(require 'ace-jump-buffer)
(require 'init-asymptote)
(require 'init-vcard)
(require 'init-auctex)
(require 'init-cdlatex)
(require 'init-google-this)
(require 'init-ebib)
;; (require 'init-dash)
;; (require 'init-ledger)
;;;;;; (require 'init-ibus)



(require 'color-theme)
;; (require 'color-theme-molokai)
;; color theme
;; (if (daemonp) ;; if run from daemon
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (select-frame frame)
;;               (color-theme-molokai)))
;;   (color-theme-molokai))

;; misc has some crucial tools I need immediately
(require 'init-misc)

(setq idle-require-idle-delay 3)
(setq idle-require-symbols '(init-compat
                             init-keyfreq
                             init-move-window-buffer
                             init-elnode
                             init-doxygen
                             init-pomodoro
                             init-emacspeak
                             init-artbollocks-mode
                             init-semantic
                             init-xterm
                             init-textile
                             init-javascript
                             init-haskell
                             init-ruby-mode
                             init-rails
                             init-sql
                             init-org2blog
                             init-csharp-mode
                             init-web-mode
                             init-lua-mode
                             init-slime))
(idle-require-mode 1) ;; starts loading

(require-package 'gnuplot)
(require-package 'lua-mode)
;; (require-package 'htmlize)
(require-package 'dsvn)
(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
;; (require 'init-locales)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (message "init completed in %.2fms"
;;                      (sanityinc/time-subtract-millis after-init-time before-init-time))))


;; (when (require 'time-date nil t)
;;   (message "Emacs startup time: %d seconds."
;;            (time-to-seconds (time-since emacs-load-start-time)))
;;   )


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
(put 'erase-buffer 'disabled nil)
