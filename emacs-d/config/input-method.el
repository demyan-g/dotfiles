;;; input-method.el --- CJK input method configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Fix for CJK input method keybinding conflicts.
;;
;; Problem: When using external input methods (Korean IME, Japanese IME),
;; Emacs keybindings like C-c C-c don't work because the IM intercepts
;; the keys before Emacs receives them (e.g., "C-ㅊ" instead of "C-c").
;;
;; Solution: sis (smart-input-source) automatically switches to English
;; input when using prefix keys, then switches back for text input.
;;
;; Prerequisites:
;; - macOS: brew tap laishulu/homebrew && brew install macism
;; - Linux with fcitx5: Already configured via D-Bus
;; - Windows: Emacs 28+ has native support

;;; Code:

;;; sis - Smart Input Source
;; (use-package sis
;;   :ensure t
;;   :config
;;   ;; Platform-specific configuration
;;   (cond
;;    ;; macOS configuration
;;    ((eq system-type 'darwin)
;;     ;; IMPORTANT: Set the external tool path explicitly
;;     ;; Install macism: brew tap laishulu/homebrew && brew install macism
;;     (setq sis-ism-external-command "macism")

;;     ;; Configure with YOUR actual input source identifiers from `macism -l`
;;     (sis-ism-lazyman-config
;;      ;; English input source
;;      "com.apple.keylayout.ABC"
;;      ;; Korean input source (change to your preferred)
;;      "com.apple.inputmethod.Korean.2SetKorean"
;;      ;; Or Japanese: "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"
;;      ;; "com.google.inputmethod.Japanese.base"
;;      ))

;;    ;; Linux with fcitx5
;;    ((and (eq system-type 'gnu/linux)
;;          (executable-find "fcitx5-remote"))
;;     (sis-ism-lazyman-config "1" "2" 'fcitx5))

;;    ;; Linux with fcitx
;;    ((and (eq system-type 'gnu/linux)
;;          (executable-find "fcitx-remote"))
;;     (sis-ism-lazyman-config "1" "2" 'fcitx))

;;    ;; Linux with ibus
;;    ((and (eq system-type 'gnu/linux)
;;          (executable-find "ibus"))
;;     (sis-ism-lazyman-config
;;      "xkb:us::eng"
;;      "hangul"  ;; Or "anthy" for Japanese
;;      'ibus))

;;    ;; Windows (Emacs 28+)
;;    ((eq system-type 'windows-nt)
;;     (sis-ism-lazyman-config nil t 'w32)))

;;   ;; Enable global modes
;;   (sis-global-cursor-color-mode t)   ;; Change cursor color based on IM state
;;   (sis-global-respect-mode t)        ;; Protect prefix keys from IM interception
;;   (sis-global-context-mode t)        ;; Smart switching based on context

;;   ;; Prefix keys to protect (switch to English when these are pressed)
;;   (setq sis-prefix-override-keys '("C-c" "C-x" "C-h" "C-u" "C-g" "M-x" "C-s"))

;;   ;; Cursor color configuration
;;   (setq sis-default-cursor-color nil)  ;; Green for English
;;   (setq sis-other-cursor-color "#ff6b6b")    ;; Red for CJK

;;   ;; Context-based switching rules
;;   (setq sis-context-hooks
;;         '(;; Switch to English in minibuffer
;;           (minibuffer-setup . sis-context-hook)
;;           ;; Switch to English when entering command
;;           (isearch-mode . sis-context-hook)))

;;   ;; Inline region auto-switch (for mixed language text)
;;   (setq sis-inline-tighten-head-rule 1)
;;   (setq sis-inline-tighten-tail-rule 1))

;;; Alternative: Emacs-native Input Methods
;; If you prefer to avoid external IM dependencies, you can use
;; Emacs-native input methods. These don't have keybinding conflicts.

;; Korean input method (built-in)
;; (setq default-input-method "korean-hangul")

;; For more sophisticated Korean input:
;; (use-package hangul
;;   :ensure nil
;;   :config
;;   (setq default-input-method "korean-hangul390"))

;; Japanese input with ddskk
;; (use-package ddskk
;;   :ensure t
;;   :bind ("C-x j" . skk-mode)
;;   :config
;;   (setq default-input-method "japanese-skk"))

;;; Function Key Fallbacks
;; If sis doesn't fully solve the problem, function keys always work
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<f5>") #'org-ctrl-c-ctrl-c)
  (define-key org-mode-map (kbd "<f6>") #'org-todo)
  (define-key org-mode-map (kbd "<f7>") #'org-schedule)
  (define-key org-mode-map (kbd "<f8>") #'org-deadline)
  (define-key org-mode-map (kbd "<f9>") #'org-export-dispatch))

;;; Helper Functions

(defun my/toggle-input-method ()
  "Toggle between English and CJK input method."
  (interactive)
  (if current-input-method
      (deactivate-input-method)
    (activate-input-method default-input-method)))

;; (defun my/switch-to-english ()
;;   "Switch to English input method."
;;   (interactive)
;;   (sis-set-english))

;; (defun my/switch-to-other ()
;;   "Switch to other (CJK) input method."
;;   (interactive)
;;   (sis-set-other))

;; (defun my/sis-status ()
;;   "Display current input source status."
;;   (interactive)
;;   (message "Input source: %s" (sis-get)))

;;; Keybindings
(global-set-key (kbd "C-\\") #'my/toggle-input-method)
(global-set-key (kbd "C-c I e") #'my/switch-to-english)
(global-set-key (kbd "C-c I o") #'my/switch-to-other)
;; (global-set-key (kbd "C-c I s") #'my/sis-status)

;;; Mode-specific Input Method Settings
;; Automatically switch to English in certain modes
;; (add-hook 'prog-mode-hook #'sis-set-english)
;; (add-hook 'minibuffer-setup-hook #'sis-set-english)

;; Keep current IM in text modes
;; (add-hook 'text-mode-hook #'sis-context-mode)
;; (add-hook 'org-mode-hook #'sis-context-mode)

;;; Debugging
;; Uncomment to debug sis issues:
;; (setq sis-log-level 'debug)

(provide 'input-method)
;;; input-method.el ends here
