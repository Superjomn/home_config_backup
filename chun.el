;; PYTHON
(defun python-env-setup () 
  "Setup the python environment"
  (progn (add-hook 'python-mode-hook 'yapf-mode) 
         (add-hook 'python-mode-hook 'python-format-bindings)))
(defun python-format-bindings () 
  (define-key python-mode-map [tab] 'yapfify-buffer))
;; <<<
