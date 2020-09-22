;; The spring mode
(require 'cl-lib)
(require 'epc)

(defgroup spring nil
  "spring group")

(defcustom spring-rpc-server-port -1
  "The port of the RPC server, if the port given is negative, create a server instead"
  :type '(integer)
  :group 'spring)

(defcustom spring-rpc-server-file "spring-server.py"
  "The path to the RPC server python script"
  :type '(string)
  :group 'spring)

(defconst spring-browser-index '(("google" . "https://www.google.com/"))
  "The browser candidates")

(message "spring-rpc-server-port: %s" spring-rpc-server-port)

(defvar spring-rpc-server nil
  "The RPC server instance")

(defun spring--initialize-rpc-server ()
  "Initialize the RPC server"
  (if (< spring-rpc-server-port 0)
      (progn (message "start a new RPC server")
             (setq spring-rpc-server (--spring--start-local-rpc-server)))
    (progn (message (format "Connect to remote RPC server with port [%d]" spring-rpc-server-port))
           (--spring--reuse-local-rpc-server))))

(defun --spring--start-local-rpc-server ()
  "Start a local RPC server"
  (epc:start-epc "python" (list spring-rpc-server-file)))

(defun --spring--reuse-local-rpc-server ()
  "Reuse a local RPC server with sepcified port"
  (setq spring-rpc-server (epc:start-epc "echo" (list (number-to-string spring-rpc-server-port))))
  (message (format "Successfully connected to localhost on port [%d]" spring-rpc-server-port)))

(spring--initialize-rpc-server)
;; (epc:call-sync spring-rpc-server 'echo '("hello"))

(defun spring-echo ()
  "Echo something to test the RPC service"
  (interactive)
  (epc:call-sync spring-rpc-server 'echo '("hello")))

(epc:call-sync spring-rpc-server 'browser_open_url '("https://www.google.com"))

(setq spring-site-map '(("google" . "https://www.google.com")
                        ("github" . "https://github.com/")
                        ("cinn" . "https://github.com/Superjomn/CINN")))

(defun --spring--dict-get (dict name)
  "get a record from a dictorary"
  (cdr (assoc name dict)))


(defun spring-open-url ()
  "browser open a url"
  (interactive)
  (let* (site url)
    (setq site (completing-read "site:" '(("google" 1)
                                          ("github" 2)
                                          ("cinn" 3)) nil t ""))
    (setq url (--spring--dict-get spring-site-map site))
    (message (format "Selected site: [%S]" url))
    (cl-assert
     (stringp url))
    (epc:call-sync spring-rpc-server 'browser_open_url (list url))))
