;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; This file runs before package.el and GUI initialization.
;; Optimizations for startup performance and native compilation.

;;; Code:

;;; Package Management
(setq package-enable-at-startup nil)

;;; Native Compilation (Emacs 29+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-deferred-compilation t)
  (setq comp-speed 3))

;; macOS Apple Silicon library paths for libgccjit
(when (and (eq system-type 'darwin)
           (string-match-p "aarch64\\|arm64" system-configuration))
  (setenv "LIBRARY_PATH"
          (string-join
           '("/opt/homebrew/opt/gcc/lib/gcc/current"
             "/opt/homebrew/opt/libgccjit/lib/gcc/current")
           ":")))

;;; UI Optimizations (before GUI loads)
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;;; Performance Optimizations
;; Maximize GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Increase process output buffer (important for LSP)
(setq read-process-output-max (* 1024 1024))

;; Disable file-name-handler during startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;;; Frame Configuration
(setq default-frame-alist
      (append default-frame-alist
              '((width . 140)
                (height . 50)
                (left . 50)
                (top . 50))))

;;; Encoding
(set-language-environment "UTF-8")

(provide 'early-init)
;;; early-init.el ends here
