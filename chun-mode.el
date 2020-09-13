;; Define a minor mode.
;; A minor mode is an optional editing mode that alters the behavior of Emacs in some well-defined way.

(defvar boolcase-mode-words '("true" "false")
  "Words to capitalize")

(defun boolcase-mode-check ()
  "Check if we capitalize or not"
  (if (= last-command-event 101) ; ascII char 'e'
      (boolcase-mode-fix)))

(defun boolcase-mode-fix ()
  (save-excursion
    (copy-region-as-kill (point)
                         (progn (backward-sexp) (point)))
    (when (member (current-kill 0) boolcase-mode-words)
      (capitalize-word 1))
    ;; Remove element we just save from kill ring
    (setq kill-ring (cdr kill-ring))
  ))

(define-minor-mode boolcase-mode
  "Automatically captalize booleans"
  :lighter " BC"

  (if boolcase-mode
      (add-hook 'post-self-insert-hook
                'boolcase-mode-check nil t)
    (remove-hook 'post-self-insert-hook
                 'boolcase-mode-check t)))
