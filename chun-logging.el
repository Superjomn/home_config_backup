;; This file contains the logging utilities, the code left here to load by all the other scripts using logging facilities.

(load-relative "./third-party/log4e.el")
(log4e:deflogger "chun" "%t [%l] %m" "%H:%M:%S")
(chun--log-enable-debugging)
(chun--log-enable-logging)

;; the log4e usage
;; (chun--log-open-log) open the logging buffer
;; (chun--log-clear-log) clear the history in the log buffer
