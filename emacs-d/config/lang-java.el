;;; lang-java.el --- Java development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; This module configures a comprehensive Java development environment using:
;; - Eclipse JDT Language Server for LSP features
;; - Support for Maven and Gradle projects
;; - JUnit test integration
;; - Lombok support
;; - Google Java Format integration

;;; Code:

;;; lsp-java
;; Eclipse JDT Language Server provides the best Java LSP experience
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook ((java-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred))
  :custom
  ;; JDT settings
  (lsp-java-server-install-dir
   (expand-file-name "eclipse.jdt.ls/" user-emacs-directory))
  (lsp-java-workspace-dir
   (expand-file-name "workspace/" user-emacs-directory))

  ;; Java version
  (lsp-java-configuration-runtimes
   '[(:name "JavaSE-17" :path "/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home" :default t)
     (:name "JavaSE-21" :path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home")])
  
  ;; Format settings
  (lsp-java-format-enabled t)
  (lsp-java-format-settings-url
   (expand-file-name "lib/google-java-format.xml" user-emacs-directory))

  ;; Import settings
  (lsp-java-import-order '["" "java" "javax" "org" "com"])
  (lsp-java-save-actions-organize-imports t)

  ;; Completion
  (lsp-java-completion-enabled t)
  (lsp-java-completion-guess-method-arguments t)

  ;; Code generation
  (lsp-java-code-generation-hash-code-equals-use-java7objects t)
  (lsp-java-code-generation-use-blocks t)
  
  :config
  ;; Enable Lombok support
  (setq lsp-java-vmargs
        `("-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx2G"
          "-Xms100m"
          ,(concat "-javaagent:"
                   (expand-file-name "lib/lombok.jar" user-emacs-directory)))))

;;; DAP Java
;; Debug Adapter Protocol for Java
(use-package dap-java
  :ensure nil
  :after (lsp-java dap-mode))

;;; Helper Functions
(defun java-run-current-main ()
  "Run the main method in the current file."
  (interactive)
  (lsp-java-run-main-class))

(defun java-run-current-test ()
  "Run tests in the current file."
  (interactive)
  (lsp-java-run-tests-file))

;; Keybindings
(with-eval-after-load 'lsp-java
  (define-key java-mode-map (kbd "C-c j r") #'lsp-java-run-main-class)
  (define-key java-mode-map (kbd "C-c j t") #'lsp-java-run-tests-file)
  (define-key java-mode-map (kbd "C-c j o") #'lsp-java-organize-imports)
  (define-key java-mode-map (kbd "C-c j g") #'lsp-java-generate-getters-and-setters))

(provide 'lang-java)
;;; lang-java.el ends here
