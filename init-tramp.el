(require 'tramp)
(setq tramp-default-method "ssh")
;; tramp-connection-timeout in second
(setq tramp-connection-timeout 600)
;; permanent password cache
(setq password-cache-expiry nil)
;; Start bash shell on the remote host
(setq explicit-shell-file-name "/bin/bash")
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\`storage2\\'"
;;                nil
;;                "/ssh:%u@%h:"))


;; Running remote programs that shall connect to the local X11 server
(add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))


(provide 'init-tramp)