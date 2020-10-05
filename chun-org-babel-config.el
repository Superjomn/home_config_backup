;; This file contains some org-babel configurations

(require 'cl-lib)
(load-relative "chun-logging")


;; auto insert code
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++"
                                       "css" "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond"
                                       "mscgen" "octave" "oz" "plantuml" "R" "sass" "screen" "sql"
                                       "awk" "ditaa" "haskell" "latex" "lisp" "matlab" "ocaml" "org"
                                       "perl" "ruby" "scheme" "sqlite")))
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
                                                         (latex . t)))




(defun chun/--org-babel-set-c++-compiler ()
  "set g++-7 for the newer compiler can compiles the modern template meta programming."
  (let* ((modern-gcc-path "")
         (gcc-candidates (list "g++-9" "g++-8" "g++-7"))
         (found nil))
    (cl-loop for candidate in gcc-candidates do
             ;;
             (progn (if (not found)
                        (progn
                          (setq modern-gcc-path (shell-command-to-string (format "which %s"
                                                                                 candidate)))
                          (if (not (string-empty-p modern-gcc-path))
                              (progn
                                (setq found t)
                                (setq org-babel-C++-compiler candidate)))))))
    (if (string-empty-p modern-gcc-path)
        (chun--log-fatal "Error, no modern C++ compiler found")
      (chun--log-warn "Found modern C++ compiler locates in %s" modern-gcc-path))))

(defun chun/--org-babel-set-default-varialbes ()
  "set default variables for org-babel C++ and python mode"
  (setq org-babel-default-header-args:C++
        '((:flags .
                  "-std=c++17")
          (:namespaces .
                       "std")
          (:includes .
                     "<iostream> <vector> <tuple> <utility> <map> <type_traits>")
          (:results .
                    "outputs"))))


;; ====================== main program ======================

(chun/--org-babel-set-c++-compiler)
(chun/--org-babel-set-default-varialbes)
