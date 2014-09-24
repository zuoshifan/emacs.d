;; Usage
;; M-x screenshot prepares to take a screenshot. It prompts for a image file name and a scheme name. Then set up screen and press C-c C-c! Emacs invokes import command (ImageMagick) and save the screenshot to local. Finally Emacs uploads it to remote server ASYNCHRONOUSLY (via scp or Yaoddmuse).
;; After taking a screenshot, the URL of the image are ready to yank. Press C-y!
;; see http://www.emacswiki.org/emacs/ScreenShot
(require 'screenshot)
(setq screenshot-schemes              ; edit as you like
      '(
        ;; To local image directory
        ("local"
         :dir "~/images/")            ; Image repository directory
        ;; To current directory
        ("current-directory"          ; No need to modify
         :dir default-directory)
        ;; To remote ssh host
        ("remote-ssh"
         :dir "/tmp/"                 ; Temporary saved directory
         :ssh-dir "www.example.org:public_html/archive/" ; SSH path
         :url "http://www.example.org/archive/")  ; Host URL prefix
        ;; To EmacsWiki (need yaoddmuse.el)
        ("EmacsWiki"                 ; Emacs users' most familiar Oddmuse wiki
         :dir "~/.yaoddmuse/EmacsWiki/"  ; same as yaoddmuse-directory
         :yaoddmuse "EmacsWiki")         ; You can specify another Oddmuse Wiki
        ;; To local web server
        ("local-server"
         :dir "~/public_html/"           ; local server directory
         :url "http://127.0.0.1/")))     ; local server URL prefix
(setq screenshot-default-scheme "local") ; default scheme is "local"




;;; Yet another function for screenshot
;; screenshot in org-mode
;; modified by gift.young@gmail.com
;; based on http://praktikanten.brueckenschlaeger.org/2010/11/28/screenshots-in-org-mode
;; see http://blog.csdn.net/younggift/article/details/6196970
(defun my-screenshot ()
"Take a screenshot into a unique-named file in the current buffer file directory and insert a link to this file.
After called this function, use the mouse to select the region you want to take a screenshot.
To use this function you need `scrot' - command line screen capture utility to be installed."
(interactive)
(setq filename
  (concat (make-temp-name
           (concat (file-name-directory (buffer-file-name)) "images/" ) ) ".png"))
(if (file-accessible-directory-p (concat (file-name-directory (buffer-file-name)) "images/"))
  nil
  (make-directory "images"))
 (call-process-shell-command "scrot" nil nil nil nil "-s" (concat "/"" filename "/"" ))
 (insert (concat "[["  filename "]]"))
 (org-display-inline-images)
)

;; (global-set-key (kbd "s-s") 'my-screenshot)

(provide 'init-screenshot)