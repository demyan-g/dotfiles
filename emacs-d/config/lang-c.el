;;; lang-c.el --- C/C++ development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; C/C++ development:
;; - clangd for LSP
;; - clang-format for formatting
;; - CMake/Meson build system support
;; - GDB/LLDB debugging
;;
;; Prerequisites:
;; - macOS: brew install llvm cmake bear
;; - Ubuntu: apt install clangd clang-format cmake bear

;;; Code:

;;; C/C++ Mode Configuration
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-or-c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :hook ((c-mode c++-mode) . lsp-deferred)
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "linux")
                          (c++-mode . "stroustrup")
                          (other . "gnu"))))

;; Tree-sitter mode remapping
(with-eval-after-load 'treesit
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;;; clangd LSP Configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-args
        '("-j=4"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=iwyu"
          "--suggest-missing-includes"
          "--pch-storage=memory"
          "--log=error")))

;;; clang-format
(use-package clang-format
  :ensure t
  :after cc-mode
  :bind (:map c-mode-map
              ("C-c f f" . clang-format-buffer)
              ("C-c f r" . clang-format-region)
         :map c++-mode-map
              ("C-c f f" . clang-format-buffer)
              ("C-c f r" . clang-format-region))
  :config
  (setq clang-format-style "file"
        clang-format-fallback-style "llvm"))

;;; CMake
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

;;; Meson
(use-package meson-mode
  :ensure t
  :mode "meson\\.build\\'")

;;; Flycheck Configuration
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(c/c++-clang c/c++-gcc c/c++-cppcheck))))

;;; compile_commands.json Generation
(defun c-generate-compile-commands-cmake ()
  "Generate compile_commands.json using CMake."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && ln -sf build/compile_commands.json .")))

(defun c-generate-compile-commands-bear ()
  "Generate compile_commands.json using Bear."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "bear -- make -j$(nproc)")))

;;; Build Commands
(defun c-cmake-configure ()
  "Configure CMake project."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")))

(defun c-cmake-build ()
  "Build CMake project."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "cmake --build build -j$(nproc)")))

(defun c-cmake-clean ()
  "Clean CMake build."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "cmake --build build --target clean")))

(defun c-cmake-test ()
  "Run CTest."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile "ctest --test-dir build --output-on-failure")))

;;; Debugging
(with-eval-after-load 'dap-mode
  (require 'dap-gdb-lldb)
  
  (dap-register-debug-template
   "C/C++::GDB Run"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
         :target nil
         :cwd "${workspaceFolder}"))
  
  (dap-register-debug-template
   "C/C++::LLDB Run"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :program nil
         :cwd "${workspaceFolder}")))

;;; Documentation
(defun c-search-cppreference ()
  "Search cppreference.com."
  (interactive)
  (let ((symbol (or (thing-at-point 'symbol) "")))
    (browse-url (format "https://en.cppreference.com/mwiki/index.php?search=%s" symbol))))

;;; Keybindings
(with-eval-after-load 'cc-mode
  (define-prefix-command 'c-build-map)
  (define-key c-mode-map (kbd "C-c C-p") 'c-build-map)
  (define-key c++-mode-map (kbd "C-c C-p") 'c-build-map)

  ;; Build commands
  (define-key c-build-map "c" #'c-cmake-configure)
  (define-key c-build-map "b" #'c-cmake-build)
  (define-key c-build-map "C" #'c-cmake-clean)
  (define-key c-build-map "t" #'c-cmake-test)

  ;; compile_commands.json generation
  (define-key c-build-map "g" #'c-generate-compile-commands-cmake)
  (define-key c-build-map "G" #'c-generate-compile-commands-bear)

  ;; Documentation
  (define-key c-build-map "d" #'c-search-cppreference)

  ;; Debugging
  (define-key c-build-map "r" #'dap-debug))

(provide 'lang-c)
;;; lang-c.el ends here
