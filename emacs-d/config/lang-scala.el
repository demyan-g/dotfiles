;;; lang-scala.el --- Scala development configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Scala development with Metals LSP server.
;; This module configures a comprehensive Scala development environment using:
;; - Metals LSP server for IDE features
;; - SBT mode for build tool integration
;; - Support for Scala 2 and Scala 3

;;; Code:

;;; Scala Mode
;; Basic Scala syntax highlighting and indentation
(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
  :hook (scala-mode . lsp-deferred)
  :interpreter ("scala" . scala-mode)
  :config
  (setq scala-indent:align-parameters t
        scala-indent:align-forms t
        scala-indent:use-javadoc-style t))

;;; SBT Mode
;; Integration with the Scala Build Tool
(use-package sbt-mode
  :ensure t
  :commands (sbt-start sbt-command)
  :bind (:map scala-mode-map
              ("C-c s s" . sbt-start)
              ("C-c s c" . sbt-command)
              ("C-c s t" . sbt-test)
              ("C-c s r" . sbt-run))
  :config
  (setq sbt:display-command-buffer nil)
  (setq sbt:program-options '("-Dsbt.supershell=false"
                              "-Dsbt.log.noformat=true"))
  
  (defun sbt-test ()
    "Run all tests in the project."
    (interactive)
    (sbt-command "test"))
  
  (defun sbt-run ()
    "Run the project."
    (interactive)
    (sbt-command "run")))

;;; Metals LSP Server
(use-package lsp-metals
  :ensure t
  :after (lsp-mode scala-mode)
  :hook ((scala-mode . lsp-deferred)
         (sbt-mode . lsp-deferred))
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"
                            "-J-Xmx2G"))
  (lsp-metals-enable-semantic-highlighting t)
  (lsp-metals-show-implicit-arguments t)
  (lsp-metals-show-implicit-conversions t)
  (lsp-metals-show-inferred-type t)
  :bind (:map scala-mode-map
              ("C-c m b" . lsp-metals-build-import)
              ("C-c m c" . lsp-metals-build-connect)
              ("C-c m d" . lsp-metals-doctor-run)))

(provide 'lang-scala)
;;; lang-scala.el ends here
