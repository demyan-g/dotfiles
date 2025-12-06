;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Fundamental Emacs configuration: encoding, backups, history, editing behavior.

;;; Code:

;;; Encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;; Directories
(defvar my/cache-dir (expand-file-name ".cache/" user-emacs-directory))
(unless (file-exists-p my/cache-dir)
  (make-directory my/cache-dir t))

;;; Backup and Auto-save
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" my/cache-dir))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" my/cache-dir) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-saves/sessions/" my/cache-dir))

(dolist (dir '("backups" "auto-saves" "auto-saves/sessions"))
  (let ((path (expand-file-name dir my/cache-dir)))
    (unless (file-exists-p path) (make-directory path t))))

(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      vc-make-backup-files t)

;;; History
(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring))
  (setq savehist-file (expand-file-name "savehist" my/cache-dir)))

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (setq recentf-exclude
        '("/tmp/" "/ssh:" "/sudo:" "\\.git/" "COMMIT_EDITMSG"
          "\\.emacs\\.d/elpa/" "\\.emacs\\.d/elpaca/"
          "recentf" ".*-autoloads\\.el\\'")))

(use-package saveplace
  :ensure nil
  :init (save-place-mode)
  :config
  (setq save-place-file (expand-file-name "places" my/cache-dir)))

;;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Pixel scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default fill-column 80)

;;; Editing Behavior
(delete-selection-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)

;; Enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Better defaults
(setq ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-emacs 'y-or-n-p
      require-final-newline t
      sentence-end-double-space nil)

;;; Line Numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;;; Highlight Current Line
(global-hl-line-mode 1)

;;; Custom File
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Global Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Window navigation
(windmove-default-keybindings)

;;; which-key (built-in Emacs 30+, package for earlier)
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.1
        which-key-sort-order 'which-key-key-order-alpha))

(provide 'core)
;;; core.el ends here
