;;; fonts.el --- Trilingual font configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Font configuration for English/Japanese/Korean using Sarasa Gothic.
;; Sarasa Gothic provides perfect 1:2 width ratio (ASCII:CJK) for code alignment.
;; Built from Inter + Iosevka + Source Han Sans.
;;
;; Install Sarasa Gothic: https://github.com/be5invis/Sarasa-Gothic/releases
;; Recommended variant: Sarasa Mono SC (or J for Japanese-primary)
;;
;; For Nerd Font icons: Install "Symbols Nerd Font Mono"
;; https://www.nerdfonts.com/

;;; Code:

;;; Font Variables
(defvar my/font-size 140
  "Default font size in 1/10 pt (140 = 14pt).")

(defvar my/font-family "Sarasa Mono SC"
  "Primary monospace font for ASCII and default.")

(defvar my/cjk-font-family "Sarasa Mono SC"
  "Font for CJK characters (Chinese, Japanese, Korean).")

(defvar my/variable-pitch-font "Sarasa UI SC"
  "Variable pitch font for prose.")

(defvar my/symbol-font "Symbols Nerd Font Mono"
  "Font for Nerd Font symbols and icons.")

;;; Font Setup Function
(defun my/setup-fonts ()
  "Configure fonts for trilingual support (English/Japanese/Korean).
Uses Sarasa Gothic for consistent 1:2 width ratio."
  (interactive)
  (when (display-graphic-p)
    ;; Default monospace font (ASCII/Latin)
    (set-face-attribute 'default nil
                        :family my/font-family
                        :height my/font-size
                        :weight 'regular)
    
    ;; Fixed pitch (for code blocks in mixed documents)
    (set-face-attribute 'fixed-pitch nil
                        :family my/font-family
                        :height my/font-size)
    
    ;; Variable pitch (for prose)
    (when (member my/variable-pitch-font (font-family-list))
      (set-face-attribute 'variable-pitch nil
                          :family my/variable-pitch-font
                          :height my/font-size))
    
    ;; CJK Scripts - Japanese
    (when (member my/cjk-font-family (font-family-list))
      ;; Hiragana
      (set-fontset-font t 'kana
                        (font-spec :family my/cjk-font-family))
      ;; Katakana (included in kana, but explicit for clarity)
      (set-fontset-font t 'katakana-jisx0201
                        (font-spec :family my/cjk-font-family))
      ;; Japanese punctuation and symbols
      (set-fontset-font t 'japanese-jisx0208
                        (font-spec :family my/cjk-font-family))
      (set-fontset-font t 'japanese-jisx0212
                        (font-spec :family my/cjk-font-family)))
    
    ;; CJK Scripts - Korean (Hangul)
    (when (member my/cjk-font-family (font-family-list))
      (set-fontset-font t 'hangul
                        (font-spec :family my/cjk-font-family)))
    
    ;; CJK Scripts - Chinese (Han characters shared by CJK)
    (when (member my/cjk-font-family (font-family-list))
      (set-fontset-font t 'han
                        (font-spec :family my/cjk-font-family))
      (set-fontset-font t 'cjk-misc
                        (font-spec :family my/cjk-font-family))
      (set-fontset-font t 'bopomofo
                        (font-spec :family my/cjk-font-family)))
    
    ;; Nerd Font symbols (for icons in modeline, dired, etc.)
    (when (member my/symbol-font (font-family-list))
      ;; Private Use Area where Nerd Fonts icons live
      (set-fontset-font t '(#xe000 . #xf8ff)
                        (font-spec :family my/symbol-font))
      ;; Extended symbols
      (set-fontset-font t 'symbol
                        (font-spec :family my/symbol-font)
                        nil 'append))
    
    ;; Emoji support (use system emoji font as fallback)
    (when (member "Apple Color Emoji" (font-family-list))
      (set-fontset-font t 'emoji
                        (font-spec :family "Apple Color Emoji")
                        nil 'prepend))
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t 'emoji
                        (font-spec :family "Noto Color Emoji")
                        nil 'append))))

;;; Performance Optimization
;; Critical for CJK fonts - prevents lag when scrolling through CJK text
(setq inhibit-compacting-font-caches t)

;;; Apply Fonts
;; For regular Emacs startup
(add-hook 'after-init-hook #'my/setup-fonts)

;; For emacsclient / daemon mode
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)

;;; Font Size Adjustment Functions
(defun my/increase-font-size ()
  "Increase font size by 10 units (1pt)."
  (interactive)
  (setq my/font-size (+ my/font-size 10))
  (my/setup-fonts)
  (message "Font size: %d" my/font-size))

(defun my/decrease-font-size ()
  "Decrease font size by 10 units (1pt)."
  (interactive)
  (setq my/font-size (max 80 (- my/font-size 10)))
  (my/setup-fonts)
  (message "Font size: %d" my/font-size))

(defun my/reset-font-size ()
  "Reset font size to default (140 = 14pt)."
  (interactive)
  (setq my/font-size 140)
  (my/setup-fonts)
  (message "Font size reset to: %d" my/font-size))

;; Keybindings for font size
(global-set-key (kbd "C-+") #'my/increase-font-size)
(global-set-key (kbd "C--") #'my/decrease-font-size)
(global-set-key (kbd "C-0") #'my/reset-font-size)

;;; Font Diagnostic Function
(defun my/font-info ()
  "Display information about current font configuration."
  (interactive)
  (message "Default: %s | CJK: %s | Size: %dpt"
           my/font-family
           my/cjk-font-family
           (/ my/font-size 10)))

(provide 'fonts)
;;; fonts.el ends here
