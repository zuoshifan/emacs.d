;; Chinese GBK coding support, solve Cheese coding error
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

;; Chinese font set
(set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono"))

(provide 'init-coding-system)