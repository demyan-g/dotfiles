;;; lsp-config.el --- LSP configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Language Server Protocol configuration:
;; - lsp-mode for core LSP functionality
;; - lsp-ui for enhanced UI
;; - DAP mode for debugging

;;; Code:

;;; lsp-mode - Core LSP Client
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  ;; General
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-log-io nil)                      ; Disable logging for performance
  (lsp-restart 'auto-restart)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-hook nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-folding t)
  (lsp-enable-imenu t)
  (lsp-enable-snippet t)
  (lsp-enable-file-watchers nil)        ; Can be slow in large projects
  
  ;; Completion
  (lsp-completion-provider :none)       ; Use corfu instead
  (lsp-completion-enable t)
  
  ;; Performance
  (lsp-idle-delay 0.5)
  (lsp-response-timeout 5)
  
  ;; Inlay hints
  (lsp-inlay-hint-enable t)
  
  :config
  ;; Use plists for better performance
  (setq lsp-use-plists t))

;;; lsp-ui - Enhanced UI
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; Sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 0.5)
  ;; Peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  ;; Doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.5)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c l d" . lsp-ui-doc-show)))

;;; DAP Mode - Debug Adapter Protocol
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  :bind (:map dap-mode-map
              ("C-c d d" . dap-debug)
              ("C-c d b" . dap-breakpoint-toggle)
              ("C-c d n" . dap-next)
              ("C-c d s" . dap-step-in)
              ("C-c d o" . dap-step-out)
              ("C-c d c" . dap-continue)
              ("C-c d r" . dap-restart-frame)
              ("C-c d q" . dap-disconnect)))

;;; Consult-LSP Integration
(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l S" . consult-lsp-file-symbols)
              ("C-c l e" . consult-lsp-diagnostics)))

(provide 'lsp-config)
;;; lsp-config.el ends here
