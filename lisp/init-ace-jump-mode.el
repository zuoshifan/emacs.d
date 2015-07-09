;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c j j") 'ace-jump-buffer)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
;;(autoload
;;  'ace-jump-mode-pop-mark
;;  "ace-jump-mode"
;;  "Ace jump back:-)"
;;  t)
;;(eval-after-load "ace-jump-mode"
;;                 -  '(ace-jump-mode-enable-mark-sync))')
;;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; ace-pinyin
(require-package 'ace-pinyin)
(ace-pinyin-global-mode +1)

;;If you use evil
;; (eval-after-load "evil" '(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))
(eval-after-load "evil" '(define-key evil-normal-state-map (kbd "SPC") 'ace-pinyin-dwim))

(provide 'init-ace-jump-mode)
