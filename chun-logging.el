;; This file contains the logging utilities, the code left here to load by all the other scripts using logging facilities.

(with-eval-after-load 'load-relative
  (load-relative "./third-party/log4e.el")
  (require 'log4e)
  (log4e:deflogger "chun" "%t [%l] %m" "%H:%M:%S")
  (chun--log-enable-debugging)
  (chun--log-enable-logging)
  (setq log4e--log-buffer-chun "*chun-log*")
  )


;; the log4e usage
;; (chun--log-open-log) open the logging buffer
;; (chun--log-clear-log) clear the history in the log buffer
