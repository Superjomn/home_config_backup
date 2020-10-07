(setq lexical-binding t)

(load-relative "./third-party/log4e.el")
(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)
(require 'aio)
(require 'spinner)
(require 'log4e)

(setq url-proxy-services '(("https" . "127.0.0.1:12333")
                           ("http" .  "127.0.0.1:12333")
                           ("no_proxy" . "^.*zilongshanren.com")))


(log4e:deflogger "leetcode" "%t [%l] %m" "%H:%M:%S" '((fatal . "FATAL")
                                                      (error
                                                       .
                                                       "ERROR")
                                                      (warn
                                                       .
                                                       "WARN")
                                                      (info . "INFO")
                                                      (debug . "DEBUG")
                                                      (trace . "TRACE")))

(setq log4e--log-buffer-leetcode "*leetcode*")

(defun leetcode-toggle-debug ()
  "Toggle debug mode"
  (interactive)
  (if (leetcode--log-debugging-p)
      (progn (leetcode--log-set-level 'info)
             (leetcode--log-disable-debugging)
             (message "leetcode disable debug"))
    (progn (leetcode--log-set-level 'debug)
           (leetcode--log-enable-debugging)
           (message "leetcode enable debug"))))


(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defvar leetcode--user nil
  "User object.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number")

(defvar leetcode--all-problems nil
  "Problems info with a list of problem objects.
The object with following attributes:
:num      Number
:tag      String
:problems List

The elements of :problems has attributes:
:status     String
:id         Number
:pos        Number
:title      String
:acceptance String
:difficulty Number {1, 2, 3}
:paid-only  Boolean {t|nil}
:tags       List")

(defvar leetcode--all-tags nil
  "ALl problems tags.")

(defvar leetcode--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar leetcode-retry-threshold 20
  "`leetcode-try` or `leetcode-submit` retry times")
(defconst leetcode--all-difficulties '("easy" "medium" "hard"))

(defconst leetcode--paid "."
  "Paid mark.")
(defconst leetcode--checkmark "Y"
  "Checkmark for accepted problem.")
(defconst leetcode--buffer-name "*leetcode*")
(defconst leetcode--description-buffer-name "*leetcode-description*")
(defconst leetcode--testcase-buffer-name "*leetcode-testcase*")
(defconst leetcode--result-buffer-name   "*leetcode-result*")

(defface leetcode-paid-face '((t
                               (:foreground "gold")))
  "Face for `leetcode--paid'."
  :group 'leetcode)

(defface leetcode-checkmark-face '((t
                                    (:foreground "#5CB85C")))
  "Face for `leetcode--checkmark'."
  :group 'leetcode)

(defface leetcode-easy-face '((t
                               (:foreground "#5CB85C")))
  "Face for easy problems."
  :group 'leetcode)

(defface leetcode-medium-face '((t
                                 (:foreground "#F0AD4E")))
  "Face for medium problems."
  :group 'leetcode)

(defface leetcode-hard-face '((t
                               (:foreground "#D9534E")))
  "Face for hard problems."
  :group 'leetcode)

;;; Login
(defconst leetcode--domain  "leetcode.com")
(defconst leetcode--base-url "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--base-url "/accounts/login"))


;; Header
(defconst leetcode--User-Agent
  '("User-Agent" .
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken      "X-CSRFToken")

;; API
(defconst leetcode--api-root                (concat leetcode--base-url "/api"))
(defconst leetcode--api-graphql             (concat leetcode--base-url "/graphql"))
(defconst leetcode--api-all-problems        (concat leetcode--api-root "/problems/all/"))
(defconst leetcode--api-all-tags            (concat leetcode--base-url "/problems/api/tags"))
;; submit
(defconst leetcode--api-submit              (concat leetcode--base-url "/problems/%s/submit/"))
(defconst leetcode--api-problems-submission (concat leetcode--base-url "/problems/%s/submit"))
(defconst leetcode--api-check-submission (concat leetcode--base-url "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode--api-try (concat leetcode-base-url  "/problems/%s/interpret_solution/"))

(defun to-list (vec)
  "Convert a Vec to list."
  (append vec '()))

(defun leetcode--refer (value)
  "Return an alist as the HTTP Referer Header."
  (cons "Referer" value))

(defun leetcode--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find (lambda (item)
                               (string= (aref item 1) "scrftoken"))
                             (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(aio-defun
  leetcode--csrf-token
  ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (aio-await (aio-url-retrieve leetcode--url-login)))
  (leetcode--maybe-csrf-token))


(defun leetcode--credentials ()
  "Receive user account and password."
  (let ((auth-source-creation-prompts '((user . "Leetcode user: ")
                                        (secret . "Leetcode password for %u: ")))
        (found (car (auth-source-search :max 1
                                        :host leetcode--domain
                                        :require '(:user :secret)
                                        :create t))))
    (if found (list (plist-get found
                               :user)
                    (let ((secret (plist-get found
                                             :secret)))
                      (if (functionp secret)
                          (funcall secret) secret)
                      (plist-get found
                                 :save-function))))))

;; (leetcode--credentials)

(defun leetcode--multipart-form-data (name value)
  "Generate multipart form data with NAME and VALUE"
  `("file" ("name" . ,name)
    ("filedata" . ,value)
    ("filename" . "")
    ("content-type" . "")))

(aio-defun
  leetcode--login
  ()
  "Steal LeetCode login session from local browser.
It also cleans LeetCode cookies in `url-cookie-file'."
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (aio-wait-for (leetcode--csrf-token))
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string my-cookies))
         (cookies-list (seq-filter (lambda (s)
                                     (not (string-empty-p s)))
                                   (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map (lambda (s)
                                   (split-string s) cookies-list)))
         (leetcode-session (cadr (assoc "LEETCODE_SESSION" cookies-pairs)))
         (leetcode-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (leetcode--DEBUG "login session: %s" leetcode-session)
    (leetcode--DEBUG "login csrftoken: %s" leetcode-csrftoken)
    (url-cookie-store "LEETCODE_SESSION" leetcode-session nil leetcode--domain "/" t)
    (url-cookie-store "csrftoken" leetcode-csrftoken nil leetcode--domain "/" t)))

(defun leetcode--login-p ()
  "Whether user is login"
  (let ((username (plist-get leetcoe--user
                             :username)))
    (and username
         (not (string-empty-p username))
         (seq-find (lambda (item)
                     (string= (aref item 1) "LEETCODE_SESSION")))
         (url-cookie-retrieve leetcode--domain "/" t))))

(defun leetcode--set-user-and-problems (user-and-problems)
  "Set `leetcode--user' and `leetcode--all-problems'.
If user isn't login, only `leetcode--all-problems' will be set.
USER-AND-PROBLEMS is an alist comes from `leetcode--api-all-problems'."
  (let-alist user-and-problems
    (setq :username .user_name
          :solved   .num_solved
          :easy     .ac_easy
          :medium   .ac_medium
          :hard     .ac_hard))
  (leetcode--DEBUG "problem status pairs: %s" .stat_status_pairs)
  ;; problem list
  (setq leetcode--all-problems (list :num .num_total
                                     :tag "all"
                                     :problems (let ((len .num_total) problems)
                                                 (dolist (i (number-sequence 0 (1- len)))
                                                   (let-alist (aref .stat_status_pairs i)
                                                     (push (list :status .status
                                                                 :id .stat.question_id
                                                                 :pos (- len i)
                                                                 :title .stat.question_title
                                                                 :acceptance (format "%.1f%%" (* 100
                                                                                                 (/
                                                                                                  (float
                                                                                                   .stat.total_acs)
                                                                                                  .stat.total_submitted)))
                                                                 :difficulty .difficulty.level
                                                                 :paid-only (eq .paid_only t))
                                                           problems))) problems))))
(aio-defun
  leetcode--fetch-user-and-problems
  ()
  "Fetch user and problems info."
  (let ((url-request-method "GET")
        (url-request-extra-headers `(,leetcode--User-Agent ,leetcode--X-Requested-With
                                                           ,(leetcode--referer
                                                             leetcode--url-login)))
        (result (aio-wait-for (aio-url-retrieve leetcode--api-all-problems))))
    (message "result: %S" result)
    (if-let ((error-info (plist-get (car result)
                                    :error)))
        (progn (switch-to-buffer (cdr result))
               (leetcode--WARN "LeetCode fetch user and problems failed: %S" error-info))
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (json-read)))))

(aio-wait-for (leetcode--fetch-user-and-problems))
