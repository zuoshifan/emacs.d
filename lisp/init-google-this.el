(setq google-this-keybind (kbd "C-x g"))
(require-package 'google-this)
(google-this-mode 1)
(setq google-base-url "http://ipv6.google.")
(setq google-location-suffix "com.hk")

(provide 'init-google-this)