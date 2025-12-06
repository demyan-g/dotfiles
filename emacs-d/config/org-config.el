;;; org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Core org-mode configuration with modern visual enhancements:
;; - org-modern for clean, minimal aesthetics
;; - org-appear for revealing hidden markup
;; - olivetti for centered writing
;; - Mixed pitch support (variable for prose, fixed for code)

;;; Code:

;;; Core Org-mode
(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :custom
  ;; Directory structure
  (org-directory "~/Documents/_org")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  
  ;; Visual settings
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-hide-leading-stars t)
  
  ;; Tags and columns
  (org-auto-align-tags nil)
  (org-tags-column 0)
  
  ;; TODO settings
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "HOLD(h@/!)"
                                 "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  
  ;; Source code settings
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  
  ;; Links
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  
  :config
  ;; Create org directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t)))

;;; org-modern - Modern Visual Styling
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  ;; Bullets
  (org-modern-star '("◉" "○" "◈" "◇" "•"))
  (org-modern-hide-stars nil)
  
  ;; Tags and timestamps
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-priority t)
  
  ;; Tables
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  
  ;; Blocks
  (org-modern-block-fringe nil)
  (org-modern-block-name t)
  
  ;; TODO keywords
  (org-modern-todo t)
  (org-modern-todo-faces
   '(("TODO" :background "#ff6b6b" :foreground "white")
     ("NEXT" :background "#4dabf7" :foreground "white")
     ("WAITING" :background "#ffa94d" :foreground "white")
     ("HOLD" :background "#868e96" :foreground "white")
     ("DONE" :background "#51cf66" :foreground "white")
     ("CANCELLED" :background "#495057" :foreground "white")))
  
  ;; Progress bars
  (org-modern-progress t)
  
  ;; Checkboxes
  (org-modern-checkbox '((88 . "☑") (45 . "◐") (32 . "☐")))
  
  ;; Horizontal rule
  (org-modern-horizontal-rule t)
  
  ;; Lists
  (org-modern-list '((43 . "◦") (45 . "–") (42 . "•"))))

;;; org-appear - Reveal Hidden Markup
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-delay 0.0)
  (org-appear-trigger 'always))

;;; org-fragtog - Auto-toggle LaTeX Fragments
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;;; olivetti - Centered Writing
(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 100)
  (olivetti-minimum-body-width 72)
  (olivetti-recall-visual-line-mode-entry-state t))

;;; Mixed Pitch - Variable/Fixed Font Mixing
(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

;; Ensure fixed-pitch for code blocks, tables, etc.
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-checkbox ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch))))))

;;; Performance Optimization
;; org-fold for faster folding (Emacs 29+)
(when (>= emacs-major-version 29)
  (setq org-fold-core-style 'overlays))

;; Caching
(setq org-element-use-cache t)

;;; Keybindings
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o l") #'org-store-link)

(provide 'org-config)
;;; org-config.el ends here
