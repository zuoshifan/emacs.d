;; Chinese GBK coding support, solve Chinese coding error
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

;; make correct display of Chinese in shell mode
(setenv "LANG" "zh_CN.UTF-8")

;; Chinese font set
(cond (*linux* (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei Mono")))
      (*is-a-mac* (set-fontset-font "fontset-default" 'han '("STHeiti"))))

;; add space between Chinese and English characters
(require-package 'pangu-spacing)
(global-pangu-spacing-mode 1)
;; space will be inserted before you save file
;;(setq pangu-spacing-real-insert-separtor t)


(provide 'init-coding-system)