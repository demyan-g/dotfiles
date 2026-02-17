;;; org-babel-config.el --- Multi-language org-babel configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Comprehensive org-babel configuration for literate programming:
;; - Python (with uv/venv support)
;; - Rust (via rustic)
;; - C/C++
;; - TypeScript/JavaScript
;; - Java
;; - Scala
;; - Shell, SQL, Emacs Lisp, LaTeX

;;; Code:

;;; Core Babel Configuration
(with-eval-after-load 'org
  ;; Load languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t)
     (java . t)
     (js . t)
     (sql . t)
     (sqlite . t)
     (latex . t)
     (plantuml . t)
     (dot . t)
     (gnuplot . t)
     (calc . t)))
  
  ;; Security: Selective confirmation
  (defun my/org-confirm-babel-evaluate (lang body)
    "Confirm evaluation for unsafe languages."
    (not (member lang '("emacs-lisp" "python" "shell" "C" "C++" "js"
                        "sql" "sqlite" "latex" "plantuml" "dot" "calc"))))
  (setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)
  
  ;; Display images after execution
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  
  ;; Default header arguments
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "both")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no"))))

;;; Python Configuration
(with-eval-after-load 'ob-python
  (setq org-babel-python-command "python3")
  
  ;; Default header args for Python
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "py")
          (:async . "yes")))
  
  ;; Helper to use uv environment
  (defun my/ob-python-use-uv ()
    "Configure ob-python to use uv environment."
    (interactive)
    (let ((venv-python (expand-file-name ".venv/bin/python"
                                         (or (projectile-project-root)
                                             default-directory))))
      (when (file-exists-p venv-python)
        (setq-local org-babel-python-command venv-python)
        (message "Using Python: %s" venv-python)))))

;;; Rust Configuration (via rustic)
(with-eval-after-load 'rustic
  (setq rustic-babel-format-src-block t
        rustic-babel-auto-wrap-main t)
  
  ;; Default header args for Rust
  (setq org-babel-default-header-args:rustic
        '((:results . "output")
          (:exports . "both"))))

;; Add rustic to babel languages after it loads
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("rustic" . rustic)))

;;; C/C++ Configuration
(with-eval-after-load 'ob-C
  (setq org-babel-C++-compiler "g++")
  
  (setq org-babel-default-header-args:C
        '((:results . "output")
          (:flags . "-std=c11 -Wall")
          (:includes . "<stdio.h>")))
  
  (setq org-babel-default-header-args:C++
        '((:results . "output")
          (:flags . "-std=c++17 -Wall")
          (:includes . ("<iostream>" "<vector>" "<string>")))))

;;; TypeScript Configuration
(use-package ob-typescript
  :ensure t
  :after org
  :config
  (setq org-babel-default-header-args:typescript
        '((:results . "output"))))

;; Alternative: ob-deno for modern TypeScript
(use-package ob-deno
  :ensure t
  :after org
  :config
  (setq org-babel-default-header-args:deno
        '((:results . "output")
          (:allow . "read,write,net"))))

;;; JavaScript Configuration
(with-eval-after-load 'ob-js
  (setq org-babel-js-cmd "node")
  (setq org-babel-default-header-args:js
        '((:results . "output"))))

;;; Java Configuration
(with-eval-after-load 'ob-java
  (setq org-babel-default-header-args:java
        '((:results . "output")
          (:dir . "."))))

;;; Scala Configuration (via Ammonite)
(use-package ammonite-term-repl
  :ensure (:host github :repo "suzzvv/ammonite-term-repl")
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (ammonite-term-repl-minor-mode t))))

(use-package ob-ammonite
  :ensure (:host github :repo "suzzvv/ob-ammonite")
  :after org ammonite-term-repl
  :config
  (setq org-babel-default-header-args:ammonite
        '((:results . "output"))
        ob-ammonite-prompt-str "scala>"))

;;; SQL Configuration
(with-eval-after-load 'ob-sql
  (setq org-babel-default-header-args:sql
        '((:results . "table")
          (:exports . "both")))
  
  ;; SQLite convenience
  (setq org-babel-default-header-args:sqlite
        '((:results . "table")
          (:colnames . "yes"))))

;;; PlantUML Configuration
(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-jar-path
        (expand-file-name "lib/plantuml.jar" user-emacs-directory))
  (setq org-babel-default-header-args:plantuml
        '((:results . "file")
          (:exports . "results"))))

;;; Graphviz (dot) Configuration
(with-eval-after-load 'ob-dot
  (setq org-babel-default-header-args:dot
        '((:results . "file")
          (:exports . "results")
          (:cmdline . "-Tpng"))))

;;; Async Execution
(use-package ob-async
  :ensure t
  :after org
  :config
  ;; ob-async for non-session blocks
  ;; Note: org-babel native :async for sessions (Org 9.5+)
  (setq ob-async-no-async-languages-alist '("ipython")))

;;; Source Block Editing
(setq org-src-ask-before-returning-to-edit-buffer nil)

;; Edit source blocks in current window
(setq org-src-window-setup 'current-window)

;; Preserve indentation in tangled files
(setq org-src-preserve-indentation t)

;;; Tangling Configuration
(setq org-babel-tangle-use-relative-file-links t)

;; Auto-tangle on save (use with caution)
(defun my/org-babel-tangle-dont-ask ()
  "Tangle without confirmation if file has #+auto_tangle: t."
  (when (and (eq major-mode 'org-mode)
             (org-collect-keywords '("auto_tangle"))
             (string= (cadar (org-collect-keywords '("auto_tangle"))) "t"))
    (org-babel-tangle)))

;; Uncomment to enable auto-tangle:
;; (add-hook 'after-save-hook #'my/org-babel-tangle-dont-ask)

;;; Results Formatting
(setq org-babel-min-lines-for-block-output 10)

;; Wrap long lines in results
(setq org-babel-results-keyword "RESULTS")

;;; Helper Functions
(defun my/org-babel-execute-buffer ()
  "Execute all source blocks in the buffer."
  (interactive)
  (org-babel-execute-buffer)
  (message "All source blocks executed."))

(defun my/org-babel-tangle-buffer ()
  "Tangle all source blocks in the buffer."
  (interactive)
  (org-babel-tangle)
  (message "Buffer tangled."))

(defun my/org-babel-remove-all-results ()
  "Remove all result blocks in the buffer."
  (interactive)
  (org-babel-remove-result-one-or-many t)
  (message "All results removed."))

;;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v C-b") #'my/org-babel-execute-buffer)
  (define-key org-mode-map (kbd "C-c C-v C-r") #'my/org-babel-remove-all-results))

(provide 'org-babel-config)
;;; org-babel-config.el ends here
