;;; lang-python.el --- Python development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Modern Python development:
;; - basedpyright for LSP
;; - ruff for linting AND formatting
;; - uv for package management
;; - envrc for virtual environment detection
;;
;; Prerequisites:
;; - uv: curl -LsSf https://astral.sh/uv/install.sh | sh
;; - basedpyright: uv tool install basedpyright
;; - ruff: uv tool install ruff

;;; Code:

;;; Python Mode
(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.pyi\\'" . python-ts-mode))
  :hook ((python-mode python-ts-mode) . (lambda ()
                                          (setq-local tab-width 4)
                                          (setq-local indent-tabs-mode nil)))
  :config
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"
        python-fill-docstring-style 'django))

;;; Virtual Environment Management
;; envrc integrates with direnv for automatic venv activation
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  :config
  (setq envrc-lighter " Env"))

;; Alternative: pyvenv for manual venv management
(use-package pyvenv
  :ensure t
  :defer t
  :commands (pyvenv-activate pyvenv-workon)
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                     ("[venv:" pyvenv-virtual-env-name "]"))))

;;; LSP with basedpyright
;; basedpyright is a fork of pyright with additional features from Pylance
(use-package lsp-pyright
  :ensure t
  :defer t
  :custom
  ;; Use basedpyright instead of pyright for extra features
  (lsp-pyright-langserver-command "basedpyright")

  ;; Type checking strictness: off, basic, standard, strict, all
  (lsp-pyright-type-checking-mode "basic")

  ;; Let ruff handle imports
  (lsp-pyright-disable-organize-imports t)

  ;; Analysis settings
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)

  ;; Inlay hints (basedpyright feature)
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types t)
  :hook ((python-mode python-ts-mode) .
         (lambda ()
           (require 'lsp-pyright)
           (lsp-deferred))))

;;; Ruff Integration
;; ruff replaces flake8, isort, black, pyupgrade, and more
;; NOTE: ruff-lsp is deprecated - use native ruff server
(use-package reformatter
  :ensure t
  :config
  ;; Define ruff formatter
  (reformatter-define ruff-format
    :program "ruff"
    :args '("format" "--stdin-filename" input-file "-")
    :lighter " RuffFmt")

  ;; Define ruff import sorter
  (reformatter-define ruff-isort
    :program "ruff"
    :args '("check" "--select" "I" "--fix" "--stdin-filename" input-file "-")
    :lighter " RuffSort"))

;; Apheleia formatters for async formatting
(with-eval-after-load 'apheleia
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--fix" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; Flycheck with ruff
;; Use ruff for on-the-fly linting
(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "Python syntax checker using ruff."
    :command ("ruff" "check"
              "--output-format=concise"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (not (any " ")))) " "
              (message (one-or-more not-newline))
              line-end))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'python-ruff)
  (setq-default flycheck-disabled-checkers
                '(python-flake8 python-pylint python-pycompile)))

;;; Testing with pytest
(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-file)
              ("C-c t F" . python-pytest-function)
              ("C-c t r" . python-pytest-repeat)
              ("C-c t p" . python-pytest))
  :custom
  (python-pytest-executable "uv run pytest")
  (python-pytest-arguments '("-v" "--tb=short")))

;;; Debugging with debugpy
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python"))

;;; uv Integration
(defun uv-sync ()
  "Run uv sync in project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "uv sync")))

(defun uv-add (package)
  "Add PACKAGE using uv."
  (interactive "sPackage to add: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "uv add %s" package))))

(defun uv-remove (package)
  "Remove PACKAGE using uv."
  (interactive "sPackage to remove: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "uv remove %s" package))))

(defun uv-run (command)
  "Run COMMAND with uv."
  (interactive "sCommand to run: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "uv run %s" command))))

;;; Documentation
(defun python-search-docs ()
  "Search Python documentation for symbol at point."
  (interactive)
  (let ((symbol (or (thing-at-point 'symbol) "")))
    (browse-url
     (format "https://docs.python.org/3/search.html?q=%s" symbol))))

;;; Keybindings
(with-eval-after-load 'python
  (define-prefix-command 'python-uv-map)
  (define-key python-mode-map (kbd "C-c C-p") 'python-uv-map)

  ;; uv commands
  (define-key python-uv-map "s" #'uv-sync)
  (define-key python-uv-map "a" #'uv-add)
  (define-key python-uv-map "r" #'uv-remove)
  (define-key python-uv-map "x" #'uv-run)

  ;; Formatting
  (define-key python-uv-map "f" #'ruff-format-buffer)
  (define-key python-uv-map "i" #'ruff-isort-buffer)

  ;; Documentation
  (define-key python-uv-map "d" #'python-search-docs))

(provide 'lang-python)
;;; lang-python.el ends here
