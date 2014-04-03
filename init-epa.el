;;; EasyPG Assistant set up

;; Use the minibuffer to enter passphrase
(setenv "GPG_AGENT_INFO" nil)

;; (require 'epa-file)
;; (epa-file-enable)

;; Always use symmetric encryption
;; (setq epa-file-encrypt-to nil)
;; Cache passphrase for symmetric encryption, not recommended to use
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Enable auto-saving when opening an encrypted file
(setq epa-file-inhibit-auto-save nil)

(provide 'init-epa)