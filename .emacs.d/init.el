;;; package --- Summary
;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;; Code:
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	    ("org" . "http://orgmode.org/elpa/")))

;; Make sure <use-package> is avaiable
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; - Ensure packages installed
(use-package magit :ensure t)
(use-package evil :ensure t)
(use-package yaml-mode :ensure t)
(use-package elpy :ensure t)
(use-package jedi :ensure t)
(use-package flycheck :ensure t)
(use-package yasnippet :ensure t)
(use-package helm :ensure t)
(use-package helm-flycheck :ensure t)
(use-package smartrep :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package powerline
  :ensure t
  :config
  (progn
    (use-package powerline-evil
      :ensure t
      :load-path "packages/powerline-evil"
      :config
      (progn
        (powerline-evil-vim-color-theme)))))

;; Yasnippet
(yas-global-mode t)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Frame / Window settings
(display-time-mode +1)
(line-number-mode +1)
(column-number-mode +1)
;; - in case, which is most of the time,
;; - init.el is loaded first starting daemon process
(defun new-frame-setup (frame)
  "Setup for new FRAME."
  (select-frame frame)
  (if (display-graphic-p frame)
      (progn
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (set-frame-parameter (selected-frame) 'alpha '(95 75))
        (set-frame-size (selected-frame) 90 46))))

;; -- Run for already-existing frames
(mapc 'new-frame-setup (frame-list))
;; -- Run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)

;; - Moving between windows in frame
(windmove-default-keybindings)
;; - Shrink / Enlarge window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)

;; Theme related
(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))
(unless (package-installed-p 'anti-zenburn-theme)
  (package-install 'anti-zenburn-theme))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Font/Encoding related
;; - UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; - Font settings
(add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono 9"))
(set-face-attribute 'default nil :font "BitStream Vera Sans Mono 9")
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Meiryo" :size 11))
(set-fontset-font t 'katakana-jisx0201 (font-spec :family "Meiryo" :size 11))

;; Indent without tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; - smart-tabs-mode related
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'python)
(add-hook 'js-mode-hook
          (lambda ()
            (smart-tabs-mode-enable)
            (smart-tambs-advice js-indent-line js-indent-level)))

;; Python-mode related
(elpy-enable)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;(use-package py-autopep8
;  :ensure t
;  :init (progn
;          (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;          (setq py-autopep8-options '("--ignore=E401"))))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)
            (setq-local electric-indent-chars (delq ?\n electric-indent-chars))
            ))

;; - smart-tabs-mode hooked
(add-hook 'python-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice python-indent-line-1 python-indent)

;; - Jedi setup
(add-hook 'python-mode-hook 'jedi:setup)
(setq elpy-rpc-backend "jedi")
(setq jedi:complete-on-dot t)

(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(define-key elpy-mode-map (kbd "C-c C-v") 'helm-flycheck)
(smartrep-define-key elpy-mode-map "C-c"
                     '(("C-n" . flycheck-next-error)
                       ("C-p" . flycheck-previous-error)))

;; YAML-mode related
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
          
;; auto-generated lines
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" default)))
 '(desktop-save-mode t)
 '(package-selected-packages
   (quote
    (powerline-evil anti-zenburn-them smart-tabs-mode python-outline smartrep helm-flycheck helm py-autopep8 flycheck company-jedi jedi yaml-mode evil 2048-game evil-mode magit flymake-python-pyflakes elpy use-package anti-zenburn-theme zenburn-theme company-statistics))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
