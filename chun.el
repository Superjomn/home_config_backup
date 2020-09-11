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
                           ("C-c r f"   . rtags-find-symbol)
                           ("C-c r r" .  rtags-rename-symbolrtags-next-match)))
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm))
  (add-hook 'c++-mode-hook 'chun/rtags))

(defun chun/semantic (MODE)
  "Custom semantic mode.
MODE: the major programming mode"
  (let ((semantic-submodes '(global-semantic-decoration-mode
                             global-semantic-idle-local-symbol-highlight-mode
                             global-semantic-highlight-func-mode global-semanticdb-minor-mode
                             global-semantic-mru-bookmark-mode global-semantic-idle-summary-mode
                             global-semantic-stickyfunc-mode)))
    (setq semantic-default-submodes (append semantic-default-submodes semantic-submodes)
          semantic-idle-scheduler-idle-time 1)
    ;; (semanticdb-enable-gnu-global-databases 'MODE)
    (semantic-mode 1)))

(defun chun/cc-base-semantic ()
  "Configuration for semantic."
  (chun/local-set-keys '(("M-RET"   .  srefactor-refactor-at-point)
                         ("C-c t"   .  senator-fold-tag-toggle)
                         ("C-."     .  semantic-symref)
                         ("M-."     .  semantic-ia-fast-jump)
                         ("C-,"     .  semantic-mrub-switch-tags)))
  (local-set-key (kbd "C-h ,") 'helm-semantic-or-imenu))

(defun chun/setup-semantic ()
  (add-hook 'c++-mode-hook '(lambda ()
                              (chun/semantic 'c++-mode)))
  (add-hook 'c++-mode-hook 'chun/cc-base-semantic))

(setq dotspacemacs-helm-position 'right)

(defun chun/turnon-semantic()
  (interactive)
  (chun/semantic 'c++-mode)
  (chun/cc-base-semantic))

(defun chun/pre-commit ()
  "run pre-commit in projectile directory"
  (interactive)
  (let* ((temp-buffer-name "*pre-commit*")
         (chun-temp-buffer)
         (cur-window))
    (setq chun-temp-buffer (generate-new-buffer temp-buffer-name))
    (message "new buffer %s" chun-temp-buffer)
    (setq cur-window (selected-window))
    (split-window-below)
    (other-window 1)
    ;; (with-current-buffer chun-temp-buffer (insert "hello world"))
    (switch-to-buffer chun-temp-buffer)
    (message "execute pre-commit in directory: %s" (projectile-project-root))

    ;; switch directory
    (let* ((default-directory (projectile-project-root)))
      (insert (shell-command-to-string "pre-commit run -a")))))
