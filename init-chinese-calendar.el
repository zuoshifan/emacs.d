(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays (append cal-china-x-important-holidays calendar-holidays))

(provide 'init-chinese-calendar)