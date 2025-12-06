;;; appearance.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Visual appearance configuration:
;; - doom-themes for modern aesthetics
;; - doom-modeline with nerd-icons
;; - Minimal, clean visual enhancements

;;; Code:

;;; Theme Configuration
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Ef theme
(use-package ef-themes
  :ensure t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-summer))

;; Zenburn theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (setq zenburn-use-variable-pitch t)
;;   (setq zenburn-scale-org-headlines t)
;;   (setq zenburn-scale-outline-headlines t)
;;   (load-theme 'zenburn t))
;; (use-package anti-zenburn-theme :ensure t)
;; (use-package nord-theme :ensure t)

;; Doom themes
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config))

;;; Nerd Icons
;; Required by doom-modeline v4.0+
;; Run M-x nerd-icons-install-fonts on first setup
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Nerd icons for dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Nerd icons for completion (Corfu)
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Nerd icons for completion (Marginalia)
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ;; Dimensions
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-window-width-limit 85)
  
  ;; Icons
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  
  ;; File name style (important for TRAMP performance)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  
  ;; What to display
  (doom-modeline-minor-modes t)      ; Show minor modes via minions
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)  ; Show encoding
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version t)
  (doom-modeline-lsp t)              ; Show LSP status
  (doom-modeline-modal-icon t)
  (doom-modeline-time t)             ; Show time
  (doom-modeline-battery t)          ; Show battery (if applicable)
  
  ;; Segment visibility
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-irc nil)
  (doom-modeline-persp-name nil))

;; Display time in modeline
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Display battery (for laptops)
(when (and (fboundp 'display-battery-mode)
           (or (eq system-type 'darwin)
               (file-exists-p "/sys/class/power_supply/BAT0")))
  (display-battery-mode 1))

;;; Minions - Collapse minor modes
(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode 1))

;;; Visual Enhancements

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF8C00")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("HACK"   . "#FFA500")
          ("NOTE"   . "#1E90FF")
          ("REVIEW" . "#7CFC00")
          ("XXX"    . "#FF4500"))))

;; Pulse on operations
(use-package pulse
  :ensure nil
  :config
  (defun my/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (cmd '(scroll-up-command scroll-down-command
                 recenter-top-bottom other-window))
    (advice-add cmd :after #'my/pulse-line)))

;;; Window Management

;; Winner mode for undo/redo window configurations
(use-package winner
  :ensure nil
  :init (winner-mode)
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo)))

;; Window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; Scrollbar in modeline (optional, minimal)
(use-package mlscroll
  :ensure t
  :config
  (setq mlscroll-width-chars 12)
  (mlscroll-mode 1))

(provide 'appearance)
;;; appearance.el ends here
