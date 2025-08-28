;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Author: SeungJun Choi
;; Version: 0.3
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This file is loaded before the package system and GUI are initialized,
;; making it the ideal place for fundamental settings that affect Emacs startup.
;; Placing these settings here provides the maximum performance benefit.
;;
;; Configured three main areas:
;; 1. Pacakge System: Disable the default `package.el` since using `elpaca`.
;; 2. UI Elements: Disable the toolbar, menu bar, and scroll bar before they
;;                 are ever drawn, which speeds up frame creation.
;; 3. Garbage Collection: Set a high garbage collection threshold during startup
;;                        to prevent GC pauses while loading the configuration.
;;                        This threshold is reset to a more reasonable value in
;;                        `init.el` after startup.
;; 4. Native Compilation: Set up environment for native compilation ahead of time.

;;; Code:

;;; Package Management Setup
;; Prevent pacakge.el from initializing as using elpaca exclusively.
;; This must be done before `pacakge-initialize` is called.
(setq package-enable-at-statup nil)

;;; Native Compilation Configuration
;; Native compilation significantly improves Emacs performance by compiling
;; Elisp to native code. On macOS with Apple Silicon, we need to configure
;; the GCC library paths for libgccjit to work properly.
(when (and (eq system-type 'darwin)
           (member system-name '("arm64-apple-darwin" "aarch64-apple-darwin")))
  ;; Set library paths for GCC and libgccjit
  ;; These paths are specific to Homebrew installation on Apple Silicon
  (setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/Cellat/gcc/15.1.0/lib/gcc/current"
           "/opt/homebrew/Cellar/gcc/15.1.0/lib/gcc/current/gcc/aarch64-apple-darwin24/15"
           "/opt/homebrew/Cellar/libgccjit/15.1.0/lib/gcc/current")
         ":")))

;; Configure native compilation ahead of time.
;; The `comp-speed` of 3 prioritizes runtime performance of compiled code.
;; comp-speed 3 = maximum optimization
;; comp-deferred-compilation = compile packages in the background
(setq comp-speed 3
      comp-deferred-compilation t
      ;; Don't display warnings from native compilation
      native-comp-async-report-warnings-errors nil)

;;; UI Optimizations
;; Disable UI elements before the first frame is drawn 
;; to avoid visual flicker and improve startup time.
;; These elements are rarely used in modern
;; Emacs workflows and can be re-enabled if needed.

;; Prevent the initial frame from being resized implicitly
(setq frame-inhibit-implied-resize t)

;; Disable the toolbar - provides GUI buttons for common commands
(tool-bar-mode -1)

;; Disable the menu bar - can be toggled back with F10 if needed
(menu-bar-mode -1)

;; Disable scroll bars
(scroll-bar-mode -1)

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Start with a black *scratch* buffer
(setq initial-scratch-message nil)

;;; Performance Optimizations
;; During startup, temporarily increase the garbage collection threshold
;; to reduce the number of garbage collections.
;; This significantly improves startup time.
;; Will reset this to a reasonable value after initialization.

;; Set a high GC threshold during startup to avoid slowdowns.
;; It will be reset to a smaller value in init.el's `emacs-startup-hook`.
(setq gc-cons-threshold (* 100 1024 1024)) ; 100MB (default is 800KB)

;; Increase the amount of data Emacs reads from processes
;; This is escpecially important for LSP servers that send large amounts of data
(setq read-process-output-max (* 1024 1024)) ; 1MB (default is 4KB)

;; Disable file name handler during startup
;; This speeds up loading of files during initialization
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file name handler after startup
(add-hook 'emacs-startup-hook
          (lambda()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;;; Frame Configuration
;; Set default frame parameters to ensure consistent appearance
(setq default-frame-alist
      '((width . 120)     ; Default frame width in characters
        (height . 80)     ; Default frame height in lines
        (left . 50)       ; Distance from left edge of screen
        (top . 50)        ; Distance from top edge of screen
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

;; Ensure using UTF-8 by default
(set-language-environment "UTF-8")

(provide 'early-init)
;;; early-init.el ends here
