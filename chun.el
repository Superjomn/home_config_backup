(defun chun/local-set-keys (key-commands)
  "Set multiple local bindings with KEY-COMMANDS list."
  (let ((local-map (current-local-map)))
    (dolist (kc key-commands)
      (define-key local-map (kbd (car kc))
        (cdr kc)))))

;; C++
(defun chun/c++-env-setup ()
  "Setup the c++ environment with following features:
- auto complete with irony
- auto error detect with flycheck
- auto compile db generation with cmake-ide
"

  ;; Bind clang-format-buffer to tab on the c++-mode only:
  (add-hook 'c++-mode-hook 'clang-format-bindings)
  (defun clang-format-bindings ()
    (define-key c++-mode-map [tab] 'clang-format-buffer))

  ;; set google c style
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  ;; Treat _ as part of the word.
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    (setq-default evil-symbol-word-search t))

  ;; config cmake-ide, which can generate the compiilation database automatically.
  ;; it can compile the project easily.
  ;; It is the key to c++ auto complete feature, works fine with irony.
  (add-hook 'c++-mode-hook '(lambda()
                              (cmake-ide-setup)))
  ;; set the build directory for cmake-ide, or it will generate random paths and lose control.
  (eval-after-load 'cmake-ide
    ;; (setq cmake-ide-build-pool-dir "/home/chunwei/project/cinn2/build")
    ;; (setq cmake-ide-build-dir "/home/chunwei/project/cinn2/build")
    (setq cmake-ide-dir "/home/chunwei/project/cinn2/build")
    (setq cmake-ide-cmake-command "-DCXX=/usr/bin/g++-8 -DCC=/usr/bin/gcc-8"))
  (defun chun/irony ()
    "Irony mode configuration."
    (interactive)

    ;; should require the dependencies first, or it will fail to setq.
    (require 'company)
    (require 'irony)
    (require 'flycheck)
    (add-hook 'irony-mode-hook 'irony-eldoc)
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-irony-c-headers)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
    (when (or (eq major-mode 'c-mode)	; Prevent from being loaded by c derived mode
	            (eq major-mode 'c++-mode))
      (irony-mode 1)))
  (add-hook 'c++-mode-hook 'chun/irony)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)

  ;; set up flycheck prompt style.
  (defun chun/flycheck ()
    "Configurate flycheck."
    (add-to-list 'display-buffer-alist `(,(rx bos "*Flycheck errors*" eos)
                                         (display-buffer-reuse-window display-buffer-in-side-window)
                                         (side            . bottom)
                                         (reusable-frames . visible)
                                         (window-height   . 0.23)))
    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
  (add-hook 'prog-mode-hook 'chun/flycheck))

(defun chun/setup-org-babel-env ()
  "org-mode babel code block"
  ;; auto insert code
  (defun org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++"
                                         "css" "calc" "asymptote" "dot" "gnuplot" "ledger"
                                         "lilypond" "mscgen" "octave" "oz" "plantuml" "R" "sass"
                                         "screen" "sql" "awk" "ditaa" "haskell" "latex" "lisp"
                                         "matlab" "ocaml" "org" "perl" "ruby" "scheme" "sqlite")))
                   (list (ido-completing-read "Source code type: " src-code-types))))
    (progn (newline-and-indent)
           (insert (format "#+BEGIN_SRC %s\n" src-code-type))
           (newline-and-indent)
           (insert "#+END_SRC\n")
           (previous-line 2)
           (org-edit-src-code)))
  (add-hook 'org-mode-hook '(lambda ()
                              ;; keybiding for insert source code
                              (local-set-key (kbd "C-c s") 'org-insert-src-block)))
  ;; add support for exectuate c++ in org-mode
  (org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                           (python . t)
                                                           (latex . t))))

;; PYTHON
(defun python-env-setup ()
  "Setup the python environment"
  (add-hook 'python-mode-hook 'yapf-mode)
  (add-hook 'python-mode-hook 'python-format-bindings)
  ;; make org-babel execute a specific python.
  (setq org-babel-python-command "python3"))

(defun chun/dash-setup()
  "Setup the dash environment to help query API docs")


(defun python-format-bindings ()
  (define-key python-mode-map [tab] 'yapfify-buffer))
;; <<<


(defun chun/--etags-setup ()
  "set up the counsel-etags"
  (progn (eval-after-load 'counsel-etags '(progn (push "cmake-build-debug"
                                                       counsel-etags-ignore-directories)
                                                 (push "cmake-build-release"
                                                       counsel-etags-ignore-directories)
                                                 (push "build" counsel-etags-ignore-directories)
                                                 (push "*.o" counsel-etags-ignore-filenames)
                                                 (push "*.py" counsel-etags-ignore-filenames)
                                                 (push "*.pyc" counsel-etags-ignore-filenames)))
         (define-key c++-mode-map [f1] 'counsel-etags-find-tag-at-point)))


(defun chun/copy-all-or-region ()
  (interactive)
  (if (use-region-p)
      (progn (kill-new
              (buffer-substring
               (region-beginning)
               (region-end)))
             (message "Text selection copied!"))
    (progn (kill-new (buffer-string))
           (message "Buffer content copied!"))))

(defun chun/highlight-current-word ()
  "Highlight the current word in the point"
  (interactive)
  (let*((word (thing-at-point 'word 'no-properties)))
    (progn (unhighlight-regexp t)
           (highlight-regexp word))))

(defun chun/setup-youdao ()
  ;; Enable Cache
  (setq url-automatic-caching t)
  (require 'youdao-dictionary))

(defun chun/insert-cpp-src ()
  (interactive)
  (insert
   "#+BEGIN_SRC C++ :includes <iostream> <vector> <algorithm> :namespaces std :flags -std=c++11 :results output\n")
  (insert "#+END_SRC\n"))

(defun chun/jump-to-header ()
  "jump to header file."
  (interactive)
  (let* ((cur-file (buffer-file-name)) suffix-pos header-file)
    (setq suffix-pos (cl-search "." cur-file
                                :from-end t))
    (setq header-file (format "%s.%s" (cl-subseq cur-file 0 suffix-pos) "h"))
    (switch-to-buffer (find-file-noselect header-file))))

(defun chun/jump-to-source ()
  "jump to source file."
  (interactive)
  (let* ((cur-file (buffer-file-name)) suffix-pos header-file)
    (setq suffix-pos (cl-search "." cur-file
                                :from-end t))
    (setq header-file (format "%s.%s" (cl-subseq cur-file 0 suffix-pos) "cc"))
    (switch-to-buffer (find-file-noselect header-file))))

(defun chun/--get-suffix (file)
  "get file suffix"
  (let* (suffix-pos)
    (setq suffix-pos (cl-search "." file
                                :from-end t))
    (substring file suffix-pos nil)))

(defun chun/jump-to-header-or-source ()
  "jump to header file if current file is source file, or else."
  (interactive)
  (let* ((cur-file (buffer-file-name))
         (suffix (chun/--get-suffix cur-file)))
    (message "suffix %s" suffix)
    (if (string-equal suffix ".h")
        (chun/jump-to-source)
      (chun/jump-to-header))))

(defun chun/format-elisp-buffer ()
  "format a elisp buffer"
  (interactive)
  (elisp-format-buffer)
  (delete-trailing-whitespace))
