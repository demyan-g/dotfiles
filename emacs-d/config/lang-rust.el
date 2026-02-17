;;; lang-rust.el --- Rust development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Rust development:
;; - rustic-mode (use emacs-rustic/rustic fork)
;; - rust-analyzer for LSP (the only modern option - RLS is deprecated)
;; - cargo integration for build/test/run
;; - clippy for linting
;; - rustfmt for formatting
;; - codelldb for debugging via DAP
;;
;; Prerequisites:
;; - rustup: > curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; - > rustup component add rust-analyzer clippy rustfmt

;;; Code:

;;; Rustic Mode
;; rustic-mode extends rust-mode with LSP integration, cargo commands,
;; and enhanced compilation buffers with ANSI color support.
;;
;; NOTE: Use the maintained fork at emacs-rustic/rustic
;; The original brotzeit/rustic is unmaintained as of May 2024.
(use-package rustic
  ;; For elpaca, use the maintained fork:
  :ensure (:host github :repo "emacs-rustic/rustic")
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . (lambda ()
                          (lsp-inlay-hints-mode)
                          (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-format-on-save t)
  (rustic-format-trigger 'on-save)
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-compile-backtrace "1")
  (rustic-lsp-client 'lsp-mode)
  (rustic-lsp-server 'rust-analyzer)
  :config
  (setq rustic-cargo-check-arguments "--all-features"))

;;; rust-analyzer LSP settings
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-expand-proc-macros t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-completion-auto-import-enable t
        lsp-rust-analyzer-completion-postfix-enable t
        lsp-rust-analyzer-diagnostics-enable t
        lsp-rust-analyzer-import-granularity "module"
        lsp-rust-analyzer-import-prefix "self"
        lsp-rust-analyzer-check-on-save t
        lsp-rust-analyzer-lru-capacity 128))

;;; Flycheck Integration
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(rust-cargo rust))))

;;; Debugging with codelldb
(with-eval-after-load 'dap-mode
  (require 'dap-codelldb)
  
  (dap-register-debug-template
   "Rust::Run"
   (list :type "lldb"
         :request "launch"
         :name "Rust::Run"
         :cargo (list :args ["build"]
                      :filter (list :name "${workspaceFolder}" :kind "bin"))
         :args []
         :cwd "${workspaceFolder}"))
  
  (dap-register-debug-template
   "Rust::Test"
   (list :type "lldb"
         :request "launch"
         :name "Rust::Test"
         :cargo (list :args ["test" "--no-run"]
                      :filter (list :name "${workspaceFolder}" :kind "test"))
         :args []
         :cwd "${workspaceFolder}")))

;;; Cargo Commands
(defun cargo-add (crate)
  "Add CRATE to Cargo.toml."
  (interactive "sCrate to add: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "cargo add %s" crate))))

(defun cargo-remove (crate)
  "Remove CRATE from Cargo.toml."
  (interactive "sCrate to remove: ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "cargo remove %s" crate))))

(defun cargo-expand ()
  "Expand macros in current file (requires cargo-expand)."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "cargo expand")))

;;; Documentation
(defun rust-search-docs ()
  "Search Rust docs for symbol at point."
  (interactive)
  (let ((symbol (or (thing-at-point 'symbol) "")))
    (browse-url (format "https://doc.rust-lang.org/std/?search=%s" symbol))))

(defun rust-search-crates ()
  "Search crates.io."
  (interactive)
  (let ((query (read-string "Search crates.io: ")))
    (browse-url (format "https://crates.io/search?q=%s" query))))

;;; Keybindings
(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "C-c C-c a") #'cargo-add)
  (define-key rustic-mode-map (kbd "C-c C-c R") #'cargo-remove)
  (define-key rustic-mode-map (kbd "C-c C-c e") #'cargo-expand)
  (define-key rustic-mode-map (kbd "C-c d d") #'rust-search-docs)
  (define-key rustic-mode-map (kbd "C-c d c") #'rust-search-crates)
  (define-key rustic-mode-map (kbd "C-c C-d") #'dap-debug))

(provide 'lang-rust)
;;; lang-rust.el ends here
