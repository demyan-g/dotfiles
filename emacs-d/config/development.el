;;; development.el --- General development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; General development tools across all languages:
;; - Flycheck for syntax checking
;; - Smartparens for pair handling
;; - YASnippet for snippets
;; - Tree-sitter for syntax highlighting
;; - Apheleia for async formatting

;;; Code:

;;; Flycheck - Syntax Checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3
        flycheck-indication-mode 'left-margin
        flycheck-highlighting-mode 'symbols
        flycheck-navigation-minimum-level 'warning)
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Smartparens - Pair Handling
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (electric-pair-mode -1)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)))

;;; YASnippet - Snippets
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs
        `(,(expand-file-name "snippets" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1)
  (setq yas-verbosity 2
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; Tree-sitter - Modern Syntax Highlighting
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Apheleia - Async Formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Diff-hl - Git gutter
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil))

(provide 'development)
;;; development.el ends here
