(require 'tramp)
(setq tramp-default-method "ssh")
;; tramp-connection-timeout in second
(setq tramp-connection-timeout 600)
;; permanent password cache
(setq password-cache-expiry nil)
;; Start bash shell on the remote host
(setq explicit-shell-file-name "/bin/bash")
;; Put auto-save files in this directory
(setq tramp-auto-save-directory "~/.backups/tramp/")
;; Chunksize for sending input to local process
(setq tramp-chunksize 8192)
;; Running remote programs that shall connect to the local X11 server
(add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))


(provide 'init-tramp)