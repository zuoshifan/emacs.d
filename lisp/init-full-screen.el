;; toggle fullscreen
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)
;; shortcut for toggle fullscreen
(global-set-key [f11] 'my-fullscreen)


(defvar goagent-proxy-py "~/program/google_appengine/goagent/local/proxy.py" "Goagent proxy.py file.")
(defvar goagent-proxy-buffer "*Goagent Proxy*" "The buffer associated with goagent proxy process.")

(defun open-goagent-proxy ()
  "Open goagent proxy."
  (if (file-exists-p goagent-proxy-py)
      (let ((shell-command-string (format "python %s 2>/dev/null" goagent-proxy-py)))
        (async-shell-command shell-command-string goagent-proxy-buffer))
    (error (format "File %s doesn't exits." goagent-proxy-py))))

(defun goagent-proxy-setup ()
  "setup for goagent proxy."
  (open-goagent-proxy)
  (delete-window (get-buffer-window goagent-proxy-buffer)))

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
;; (add-hook 'window-setup-hook 'goagent-proxy-setup t)

(provide 'init-full-screen)