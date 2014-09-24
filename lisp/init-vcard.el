;; (require 'vcard-mode)

;; load vcard-mode until editing a vCard file
(autoload 'vcard-mode "vcard-mode" "Major mode for vCard files" t)
(add-to-list 'auto-mode-alist '("\\.vc\\(f\\|ard\\)\\'" . vcard-mode))

(provide 'init-vcard)