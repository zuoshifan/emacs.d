;; Chinese GBK coding support, solve Cheese coding error
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

;; Chinese font set
(cond (*linux* (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono")))
      (*is-a-mac* (set-fontset-font "fontset-default" 'han '("STHeiti"))))

(provide 'init-coding-system)