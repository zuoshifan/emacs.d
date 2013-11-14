;; toggle fullscreen
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)
;; shortcut for toggle fullscreen
(global-set-key [f11] 'my-fullscreen)

(defun fullscreen-split-horizontally ()
  "Make emacs full screen and horizontally spit the window."
  (interactive)
;;;;;; (toggle-frame-fullscreen) ;; not available in version z4.3.1
  (my-fullscreen)
  (split-window-horizontally-instead))

;; First maximize the frame, then make it full screen and split the window horizontally. [F11] can toggle between maximized-frame and full screen state.
;; max frame, @see https://github.com/rmm5t/maxframe.el
(require 'maxframe)
;; (setq mf-max-width 1600) ;; Pixel width of main monitor. for dual-lcd only
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'fullscreen-split-horizontally t)

(provide 'init-full-screen)