;; The spring mode
(require 'cl-lib)

(defgroup spring nil
  "spring group")

(defcustom spring-rpc-server-port -1
  "The port of the RPC server, if the port given is negative, create a server locally instead"
  :type '(integer)
  :group 'spring)

(defconst spring-browser-index '(("google" . "https://www.google.com/"))
  "The browser candidates")

(message "spring-rpc-server-port: %s" spring-rpc-server-port)

(defvar spring-rpc-server nil
  "The RPC server instance")

(defun spring--initialize-rpc-server ()
  "Initialize the RPC server"
  (if (> spring-rpc-server-port 0)
      (message "reuse RPC server")
    (message "re-start a local RPC server")))

(spring--initialize-rpc-server)
