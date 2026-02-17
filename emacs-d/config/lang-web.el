;;; lang-web.el --- TypeScript/JavaScript development -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; This module configures a modern TypeScript/JavaScript development environment using:
;;
;; - typescript-language-server (ts-ls) for LSP
;; - tree-sitter modes for syntax highlighting (Emacs 29+)
;; - pnpm for package management
;; - prettier for formatting
;; - ESLint for linting (via eslint_d for speed)
;;
;; Why combined file (not separate lang-typescript.el / lang-javascript.el)?
;; - Same LSP server handles both
;; - Same formatter (prettier) for both
;; - Same linter (ESLint) for both
;; - JSX/TSX share the same mode
;; - Reduces configuration duplication
;;
;; Prerequisites:
;; - pnpm: npm install -g pnpm (or corepack enable)
;; - typescript-language-server: pnpm add -g typescript typescript-language-server
;; - prettier: pnpm add -g prettier (or project-local)
;; - eslint_d: pnpm add -g eslint_d (faster ESLint daemon)

;;; Code:

;;; Tree-sitter Mode Configuration
(use-package treesit
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode))
  :config
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                  (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))))

;;; LSP Configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-typescript-suggest-complete-function-calls t
        lsp-typescript-format-enable nil
        lsp-javascript-format-enable nil
        lsp-typescript-inlay-hints-parameter-names-enabled "all"
        lsp-typescript-inlay-hints-parameter-types-enabled t
        lsp-typescript-inlay-hints-variable-types-enabled t))

(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)

;;; Prettier Integration
(with-eval-after-load 'apheleia
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier))

;;; ESLint Integration
(with-eval-after-load 'flycheck
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint javascript-jslint))))

;;; pnpm Integration
(defun pnpm-install ()
  "Run pnpm install."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "pnpm install")))

(defun pnpm-add (package)
  "Add PACKAGE using pnpm."
  (interactive "sPackage to add: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "pnpm add %s" package))))

(defun pnpm-add-dev (package)
  "Add PACKAGE as dev dependency using pnpm."
  (interactive "sPackage to add (dev): ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "pnpm add -D %s" package))))

(defun pnpm-remove (package)
  "Remove PACKAGE using pnpm."
  (interactive "sPackage to remove: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "pnpm remove %s" package))))

(defun pnpm-run (script)
  "Run SCRIPT using pnpm."
  (interactive
   (list (completing-read "Script: " (pnpm--get-scripts))))
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "pnpm run %s" script))))

(defun pnpm-dev ()
  "Run pnpm dev (common dev server command)."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "pnpm dev")))

(defun pnpm-build ()
  "Run pnpm build."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "pnpm build")))

(defun pnpm-test ()
  "Run pnpm test."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "pnpm test")))

(defun pnpm--get-scripts ()
  "Get scripts from package.json."
  (let* ((default-directory (or (projectile-project-root) default-directory))
         (package-json (expand-file-name "package.json" default-directory)))
    (when (file-exists-p package-json)
      (let* ((json-object-type 'alist)
             (json (json-read-file package-json))
             (scripts (alist-get 'scripts json)))
        (mapcar (lambda (s) (symbol-name (car s))) scripts)))))

;;; Node.js Debugging with DAP
(with-eval-after-load 'dap-mode
  (require 'dap-node)
  
  (dap-register-debug-template
   "Node::Run"
   (list :type "node"
         :request "launch"
         :name "Node::Run"
         :program "${workspaceFolder}/index.js"
         :cwd "${workspaceFolder}"))
  
  (dap-register-debug-template
   "Node::TypeScript"
   (list :type "node"
         :request "launch"
         :name "Node::TypeScript"
         :runtimeArgs ["-r" "ts-node/register"]
         :program "${workspaceFolder}/src/index.ts"
         :cwd "${workspaceFolder}")))

;;; Documentation
(defun web-search-mdn ()
  "Search MDN Web Docs for symbol at point."
  (interactive)
  (let ((symbol (or (thing-at-point 'symbol) "")))
    (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s" symbol))))

(defun web-search-npm ()
  "Search npm for a package."
  (interactive)
  (let ((query (read-string "Search npm: ")))
    (browse-url (format "https://www.npmjs.com/search?q=%s" query))))

;;; Keybindings
(defvar web-mode-prefix-map (make-sparse-keymap)
  "Prefix keymap for web development commands.")

;; pnpm commands
(define-key web-mode-prefix-map "i" #'pnpm-install)
(define-key web-mode-prefix-map "a" #'pnpm-add)
(define-key web-mode-prefix-map "A" #'pnpm-add-dev)
(define-key web-mode-prefix-map "r" #'pnpm-remove)
(define-key web-mode-prefix-map "s" #'pnpm-run)
(define-key web-mode-prefix-map "d" #'pnpm-dev)
(define-key web-mode-prefix-map "b" #'pnpm-build)
(define-key web-mode-prefix-map "t" #'pnpm-test)

;; Documentation
(define-key web-mode-prefix-map "m" #'web-search-mdn)
(define-key web-mode-prefix-map "n" #'web-search-npm)

;; Debugging
(define-key web-mode-prefix-map "D" #'dap-debug)

;; Apply to modes
(with-eval-after-load 'treesit
  (when (boundp 'typescript-ts-mode-map)
    (define-key typescript-ts-mode-map (kbd "C-c C-p") web-mode-prefix-map))
  (when (boundp 'tsx-ts-mode-map)
    (define-key tsx-ts-mode-map (kbd "C-c C-p") web-mode-prefix-map))
  (when (boundp 'js-ts-mode-map)
    (define-key js-ts-mode-map (kbd "C-c C-p") web-mode-prefix-map)))

(provide 'lang-web)
;;; lang-web.el ends here
