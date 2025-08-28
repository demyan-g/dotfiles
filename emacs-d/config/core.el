;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;; Author: SeungJun Choi <seungjun.choi@demyan.io>
;; Version: 0.3
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This module contains fundamental Emacs setting sthat affect basic
;; befavior across all modes and usage patterns.
;;
;; Includes:
;; - File handling (backups, auto-saves, lockfiles)
;; - Encoding configuration (UTF-8, everywhere)
;; - Basic editing behavior (tabs, indentation)
;; - Display settings (time, line numbers)
;; - Window management keybindings
;; - Platform-specific configurations (especially macOS)
;; - Dired enhancements
;;
;; These settings form the foundation upon which other modules build.

;;; Code:

;;; File Handling
;; Configure how Emacs handles file backups and auto-saves
;; - mostly not creating unnecessary files during editing 
(setq make-backup-files nil     ; Don't create backup~ files
      auto-save-default nil     ; Don't create #autosave# files
      create-lockfiles nil)     ; Don't create .#lock files

;; In case of enabling backups with a centralized location became needed:
;; (setq backup-directory-alist
;;       `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
;;       auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;;; Encoding Configuration
;; Ensure UTF-8 is used everywhere to prevent encoding issues
;; Especially important for international users and
;; when working with files containing non-ASCII characters
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf8)

;; Treat clipboard input as UTF-8
(setq x-select-request-type '(UTF_STEING COMPOUND_TEXT TEXT STRING))

;;; Preferable Defaults

;; Replace yes/noe prompts with y/n for faster interaction
(fset 'yes-or-no-p 'y-or-n-p)

;; Default indentation settings
;; Use spaces instead of tabs for consistency across environments
(setq-default tab-width 4               ; Display tabs as 4 spaces
              indent-tabs-mode nil)     ; Use spaces for indentation

;; Enable useful minor modes
(global-auto-revert-mode 1)             ; Auto-refresh buffers when files change
(delete-selection-mode 1)               ; Replace selection when typing
(save-place-mode 1)                     ; Remember cursor position in files
(recentf-mode 1)                        ; Track recently opened files

;; Increase the number of recent files tracked
(setq recentf-max-menu-items 25
      recentf-max-saved-items 100)

;;; Display Settings
;; Configure how information is displayed in the mode line and buffer

(display-time-mode +1)                  ; Show time in mode line
(line-number-mode +1)                   ; Show line number in mode line
(column-number-mode +1)                 ; Show column number in mode line
(setq display-time-24hr-format t        ; Use 24-hour time format
      diaplay-time-default-load-average nil) ; Don't show load average

;; Show line numbers in programming modes
;; - display-line-numbers is faster than the older linum-mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Window Management
;; Enhanced window navigation using Shift + Arrow keys
;; This is more intuitive than the default C-x o cycling

(windmove-default-keybindings)          ; Navigate windows with Shift+arrows

;; Windows resizing keybindings
;; These allow quick window size adjustments without M-x commands
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)

;; Quick window splitting
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;;; Scrolling Behavior
;; Make scrolling smoother and more predictable
(setq scroll-margin 3                   ; Start scrolling 3 lines before edge
      scroll-conservatively 101         ; Scroll one line at a time
      scroll-preserve-screen-position t ; Keep point position when scrolling
      mouse-wheel-progressive-speed nil ; Don't accelerate mouse scrolling
      mouse-wheel-scroll-amount '(3))   ; Scroll 3 lines with mouse wheel

;;; macOS Specific Configuration
(when (eq system-type 'darwin)
  ;; Configure modifier keys for macOS
  ;; This matches common macOS application behavior
  (setq mac-option-modifier 'meta       ; Option key is Meta
        mac-command-modifier 'super     ; Command key is Super
        ;; mac-right-option-modifier nil   ; Right Option for accented charactoers
        )

  ;; Enable native fullscreen mode
  (setq ns-use-native-fullscreen t)

  
  )
