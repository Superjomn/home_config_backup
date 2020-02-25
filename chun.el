(defun chun/local-set-keys (key-commands)
  "Set multiple local bindings with KEY-COMMANDS list."
  (let ((local-map (current-local-map)))
    (dolist (kc key-commands)
      (define-key local-map (kbd (car kc))
        (cdr kc)))))

;; PYTHON
(defun python-env-setup ()
  "Setup the python environment"
  (progn (add-hook 'python-mode-hook 'yapf-mode)
         (add-hook 'python-mode-hook 'python-format-bindings)))
(defun python-format-bindings ()
  (define-key python-mode-map [tab] 'yapfify-buffer))
;; <<<

(defun chun/format-elisp-buffer ()
  (interactive)
  (elisp-format-buffer)
  (delete-trailing-whitespace))

(defun chun/setup-rtags ()
  (use-package
    rtags
    :commands rtags-start-process-unless-running
    :config (progn (message "Rtags loaded")
                   (use-package
                     company-rtags)))
  (defun chun/rtags ()
    "Rtags configuration.
Used only for nevigation."
    (interactive)
    (rtags-start-process-unless-running)
    (setq rtags-display-result-backend 'helm)
    (chun/local-set-keys '(("C-c r ."     .  rtags-find-symbol-at-point)
                           ("C-c r ?"     .  rtags-find-references-at-point)
                           ("C-c r h"     .  rtags-location-stack-back)
                           ("C-c r l"   .    rtags-location-stack-forward)
                           ("C-c r r" .  rtags-rename-symbolrtags-next-match)))
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)))
