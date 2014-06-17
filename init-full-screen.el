;; toggle fullscreen
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)
;; shortcut for toggle fullscreen
(global-set-key [f11] 'my-fullscreen)


(defun open-goagent-proxy ()
  "Open goagent proxy."
  (shell-command "python ~/program/google_appengine/goagent/local/proxy.py > /dev/null 2>&1 &"))

(defun goagent-proxy-setup ()
  "setup for goagent proxy."
  (open-goagent-proxy)
  (delete-window (get-buffer-window "*Async Shell Command*")))

;; First maximize the frame, then make it full screen and split the window horizontally. [F11] can toggle between maximized-frame and full screen state.
;; max frame, @see https://github.com/rmm5t/maxframe.el
(require 'maxframe)
;; (setq mf-max-width 1600) ;; Pixel width of main monitor. for dual-lcd only
(add-hook 'window-setup-hook 'maximize-frame t)
;; full screen
(add-hook 'window-setup-hook 'my-fullscreen t)
;; display the org agenda when set up
(add-hook 'window-setup-hook 'org-agenda-list t)
;; goagent proxy setup
(add-hook 'window-setup-hook 'goagent-proxy-setup t)

(provide 'init-full-screen)