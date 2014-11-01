(require-package 'dired+)
(require-package 'dired-sort)

(require 'dired-details)
(dired-details-install)

;; Sort Directories First
(setq dired-listing-switches "-aBhl  --group-directories-first")

(defun dired-nautilus ()
  "Load current directory with nautilus."
  (interactive)
  (shell-command
   (concat "nautilus \"" (dired-current-directory) "\"")))
(eval-after-load "dired"
  '(define-key dired-mode-map "\C-d" 'dired-nautilus))

(defun dired-get-size ()
  "Get total size of marked files with `du' command.
If not marked any files, default is current file or directory."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*\\(total\\|总用量\\)$")
                 (match-string 1))))))

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

;; @see http://blog.twonegatives.com/post/19292622546/dired-dwim-target-is-j00-j00-magic
;; op open two new dired buffers side-by-side and give your new-found automagic power a whirl.
;; Now combine that with a nice window configuration stored in a register and you’ve got a pretty slick work flow.
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

(eval-after-load 'dired
  '(progn
     ;; {dired-details
     (setq-default dired-details-hidden-string "")
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     ;; }
     (define-key dired-mode-map "/" 'diredext-exec-git-command-in-shell)

     (require 'dired+)
     (require 'dired-sort)
     (when (fboundp 'global-dired-hide-details-mode)
       (global-dired-hide-details-mode -1))
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (add-hook 'dired-mode-hook
               (lambda () (guide-key/add-local-guide-key-sequence "%")))
     (dolist (file `(((if *unix* "zathura" "open") "pdf" "dvi" "pdf.gz" "ps" "eps")
                     ("unrar x" "rar")
                     ((if *unix* "mplayer -stop-xscreensaver" "mplayer")  "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                     ("mplayer -playlist" "list" "pls")
                     ((if *unix* "feh" "open") "gif" "jpeg" "jpg" "tif" "png" )
                     ("7z x" "7z")
                     ("djview" "djvu")
                     ("firefox" "xml" "xhtml" "html" "htm" "mht")))
       (add-to-list 'dired-guess-shell-alist-default
                    (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))
     ))


;; Use adobe reader to open PDF file
(eval-after-load 'dired+
  '(progn
     (add-to-list 'dired-guess-shell-alist-user '("\\.pdf$" "acroread * &"))
     (add-to-list 'dired-guess-shell-alist-user '("\\.ps$" "evince * &"))))
;; Print PDF file using shell command
;; lpr -P HP-LaserJet-P2055d -o Quality=FromPrintoutMode -o PageSize=A4 -o PageRegion=A4 -o PrintoutMode=Normal -o inputSlot=Default -o Duplex=DuplexNoTumble "file.pdf"

(provide 'init-dired)
