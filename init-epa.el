;;; EasyPG Assistant set up

;; Use the minibuffer to enter passphrase
(setenv "GPG_AGENT_INFO" nil)
;; @see http://www.emacswiki.org/emacs/EasyPG#toc4
;; besides, use gnupg 1.4.9 instead of 2.0
(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))


;; (require 'epa-file)
;; (epa-file-enable)

;; Always use symmetric encryption
;; (setq epa-file-encrypt-to nil)
;; Cache passphrase for symmetric encryption, not recommended to use
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Enable auto-saving when opening an encrypted file
(setq epa-file-inhibit-auto-save nil)

(provide 'init-epa)