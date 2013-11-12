(defun fullscreen-split-horizontally ()
  "Make emacs full screen and horizontally spit the window."
  (interactive)
  (toggle-frame-fullscreen)
  (split-window-horizontally-instead))

;; First maximize the frame, then make it full screen and split the window horizontally. [F11] can toggle between maximized-frame and full screen state.
;; max frame, @see https://github.com/rmm5t/maxframe.el
(require 'maxframe)
;; (setq mf-max-width 1600) ;; Pixel width of main monitor. for dual-lcd only
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'fullscreen-split-horizontally t)

(provide 'init-full-screen)