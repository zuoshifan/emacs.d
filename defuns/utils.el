;;; useful macros copyed from http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html?source=rss
;;; see also http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
(defmacro run (exec)
  "Return a function that runs the executable EXEC."
  (let ((func-name (intern (concat "endless/run-" exec))))
    `(progn
       (defun ,func-name ()
         ,(format "Run the %s executable." exec)
         (interactive)
         (start-process "" nil ,exec))
       ',func-name)))

;; (define-key launcher-map "m" (run "Mathematica"))
;; (define-key launcher-map "k" (run "keepass"))
;; (define-key launcher-map "v" (run "steam"))



(defmacro browse (url)
  "Return a function that calls `browse-url' on URL."
  (let ((func-name (intern (concat "endless/browse-" url))))
    `(progn
       (defun ,func-name ()
         ,(format "Browse to the url %s." url)
         (interactive)
         (browse-url ,url))
       ',func-name)))

;; (define-key launcher-map "r" (browse "http://www.reddit.com/r/emacs/"))
;; (define-key launcher-map "w" (browse "http://www.emacswiki.org/"))
;; (define-key launcher-map "?" (browse "http://emacs.stackexchange.com/"))
;; (define-key launcher-map "+" (browse "https://plus.google.com/communities/114815898697665598016"))