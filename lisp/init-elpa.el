;;; Find and load the correct package.el

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir
       (expand-file-name "site-lisp/package" user-emacs-directory)))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local %s package.el from load-path to avoid shadowing bundled version" package-el-site-lisp-dir)
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)


;;------------------------------------------------------------------------------
;; Patch up annoying package.el quirks
;;------------------------------------------------------------------------------

(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))


;;------------------------------------------------------------------------------
;; Add support to package.el for pre-filtering available packages
;;------------------------------------------------------------------------------

(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            ;; (funcall package-filter-function
            ;;          (car package)
            ;;          (package-desc-vers (cdr package))
            ;;          archive))
       (funcall package-filter-function
          (car package)
          (funcall (if (fboundp 'package-desc-version) 
           'package--ac-desc-version
         'package-desc-vers) 
            (cdr package)) 
          archive))
    ad-do-it))


;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))



;;; Standard package repositories

;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; We include the org repository for completeness, but don't normally
;; use it.
;; lock org-mode temporarily
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))



;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)
(after-load 'init-exec-path
  (sanityinc/package-maybe-enable-signatures))


;; well, melpa does not bother supporting emacs23 any more, but cl-lib is still required
;; TODO: in half a year, I will remove gnu elpa because emacs 24.3 is the minimum version
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")
;;                          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
;;                          ))

;; Un-comment below line if you download zip file from https://github.com/redguardtoo/myelpa/archive/master.zip and extract its content into ~/myelpa/
;; (setq package-archives '(("myelpa" . "~/myelpa")))

;; Or Un-comment below line if you prefer installing package from https://github.com/redguardtoo/myelpa/ directly
;; (setq package-archives '(("myelpa" . "https://raw.github.com/redguardtoo/myelpa/master/")))

(defvar melpa-include-packages
  '(bbdb
    xml-rpc
    json-rpc
    kv
    color-theme
    wgrep
    robe
    inf-ruby
    dsvn
    move-text
    findr
    mwe-log-commands
    dired-details
    yaml-mode
    noflet
    db
    creole
    web
    elnode
    sass-mode
    idomenu
    pointback
    buffer-move
    regex-tool
    csharp-mode
    switch-window
    cmake-mode
    sr-speedbar
    smartparens
    quack
    iedit
    legalese
    htmlize
    scratch
    mic-paren
    session
    crontab-mode
    bookmark+
    flymake-lua
    multi-term
    dired+
    inflections
    dropdown-list
    lua-mode
    anaconda-mode
    pomodoro
    helm
    auto-compile
    packed
    gitconfig-mode
    project-local-variables
    org-fstree
    todochiku
    textile-mode
    pretty-mode
    auto-complete-clang
    w3m
    fakir
    erlang
    fancy-narrow
    fullframe
    guide-key
    ipretty
    redshank
    highlight-escape-sequences
    git-blame
    git-rebase-mode
    github-browse-file
    bug-reference-github
    github-clone
    alert
    gntp
    css-eldoc
    hayoo
    json
    hi2
    ghci-completion
    sql-indent
    dired-sort)
  "Don't install any Melpa packages except these packages")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (and (string-equal archive "melpa") (memq package melpa-include-packages))
             (not (string-equal archive "melpa")))
         )))

;; un-comment below code if you prefer use all the package on melpa (unstable) without limitation
;; (setq package-filter-function nil)

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;;; Fire up package.el and ensure the following packages are installed.

(setq package-enable-at-startup nil)
(package-initialize)



(require-package 'fullframe)
(fullframe list-packages quit-window)


(require-package 'cl-lib)
(require 'cl-lib)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (cl-loop for column across tabulated-list-format
           when (string= col-name (car column))
           do (setf (elt column 1) width)))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

(require-package 'all)
(require-package 'cl-lib '(0 0 5) nil)
(require-package 'xml-rpc)
(require-package 'kv '(0 0 19) nil)
(require-package 'dash '(2 5 0) nil)
; color-theme 6.6.1 in elpa is buggy
(require-package 'color-theme)
;;;;;; (require-package 'color-theme-molokai)
(require-package 'auto-compile)
(require-package 'ace-jump-mode)
(require-package 'expand-region '(0 8 0) nil)
(require-package 'fringe-helper)
(require-package 'gnuplot)
(require-package 'haskell-mode '(13 7 0) nil)
(require-package 'magit '(1 2 0) nil)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'wgrep)
(require-package 'lua-mode)
(require-package 'project-local-variables)
(require-package 'robe)
(require-package 'inf-ruby '(2 3 0) nil)
(require-package 'yaml-mode)
(require-package 'paredit)
(require-package 'erlang '(20120612 0 0) nil)
(if *emacs24* (require-package 'browse-kill-ring))
(require-package 'findr)
(require-package 'jump '(2 3 0) nil)
(require-package 'haml-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'markdown-mode)
(require-package 'dired+)
(require-package 'maxframe)
(require-package 'org-fstree)
(require-package 'htmlize)
;;;;;; (require-package 'org2blog '(20130115 2217 0) nil)
;;;;;; (require-package 'clojure-mode)
;;;;;; (require-package 'clojure-test-mode)
;;;;;; (require-package 'cljsbuild-mode)
;;;;;; (require-package 'nrepl)
;;;;;; (require-package 'slamhound)
(require-package 'diminish)
;;;;;; (require-package 'php-mode)
;;;;;; (require-package 'smarty-mode)
(require-package 'scratch)
(require-package 'mic-paren)
(require-package 'rainbow-delimiters)
(require-package 'todochiku)
(require-package 'marmalade)
(require-package 'textile-mode)
(require-package 'pretty-mode)
(if *emacs24* (require-package 'coffee-mode))
(require-package 'crontab-mode)
(require-package 'dsvn)
(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'flymake-coffee)
(require-package 'flymake-css)
(require-package 'flymake-haml)
(require-package 'flymake-jslint)
(require-package 'flymake-python-pyflakes)
(require-package 'flymake-ruby)
(require-package 'flymake-sass)
(require-package 'flymake-shell)
(require-package 'hl-sexp)
(require-package 'ibuffer-vc)
(require-package 'less-css-mode)
(require-package 'move-text)
(require-package 'mwe-log-commands)
(require-package 'page-break-lines)
(require-package 'pointback)
(require-package 'regex-tool)
(require-package 'rinari)
(require-package 'ruby-compilation)
(require-package 'csharp-mode)
(require-package 'cmake-mode)
;;;;;; (require-package 'fuzzy)
;;;;;; (require-package 'auto-complete) ;auto-complete is dependent on fuzzy
;;;;;; (require-package 'auto-complete-clang)
(require-package 'emmet-mode)
(require-package 'session)
;; (require-package 'tidy)
(require-package 'unfill)
(require-package 'auctex)
;;evil-20120725 requires ert
;;;;;;; (require-package 'evil '(1 0 3) nil)
;;;;;;; (require-package 'evil-leader '(20130316 1414 0) nil)
;;;;;;; (require-package 'evil-numbers '(20120712 1933 0) nil)
(require-package 'w3m)
(require-package 'idomenu)
(require-package 'ctags)
(require-package 'ggtags)
(require-package 'buffer-move)
(require-package 'switch-window)
(require-package 'maxframe)
(require-package 'cpputils-cmake '(0 4 17) nil)
(require-package 'flyspell-lazy)
(require-package 'bbdb '(20130421 1145 0) nil)
(require-package 'iedit)
(require-package 'pomodoro '(20130114 1543 0) nil)
(require-package 'flymake-lua)
(require-package 'dropdown-list)
(if *emacs24* (require-package 'yasnippet '(0 9 0 1) nil))
;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'bookmark+)
(require-package 'multi-term)
(require-package 'json-mode)
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 1))
    (require-package 'js2-mode '(20140114 0 0) nil)
  )
(require-package 'tagedit)
(require-package 'fancy-narrow)
(require-package 'sr-speedbar)
(require-package 'smartparens)
;; company-mode drop emacs 23 support
(if (>= emacs-major-version 24) (require-package 'company '(0 8 5) nil))
(require-package 'legalese)
(require-package 'string-edit)
(require-package 'dired-details)
(require-package 'ag)
(if *emacs24* (require-package 'git-gutter '(0 71) nil))
(require-package 'fakir)
(require-package 'f)
(require-package 'elnode) ;; elnode dependent on f
(when *emacs24*
  (require-package 'anaconda-mode))
(require-package 'quack) ;; for scheme
;; (require-package 'git-messenger '(20130613 1222 0) nil)
;; (require-package 'issue-tracker '(0 0 1) nil)

;; (require-package 'command-frequency)
(require-package 'cdlatex)
(require-package 'ebib)
;; (require-package 'pymacs)
(require-package 'eimp)
(require-package 'vcard)
;; (require-package 'google-this)
(require-package 'google-translate)
;; (require-package 'jedi)
(require-package 'cython-mode)
;; (require-package 'predictive)

(provide 'init-elpa)
