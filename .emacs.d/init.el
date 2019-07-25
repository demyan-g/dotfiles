;;; init.el --- Emacs initial configuration file

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
(use-package paradox
  :ensure t
  :custom
  (paradox-github-token t))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "<img draggable=\"false\" class=\"emoji\" alt=\"⚫\" src=\"https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/26ab.svg\">" "○" "►" "◇"))
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "|" "<img draggable=\"false\" class=\"emoji\" alt=\"✔\" src=\"https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/2714.svg\"> DONE(d)")
          (sequence "⚑ WAITING(w)" "|")
          (sequence "|" "✘ CANCELED(c)")))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package plantuml-mode :ensure t)
(use-package company :ensure t)
(use-package magit :ensure t)
;; (use-package evil :ensure t)
(use-package yaml-mode :ensure t)
(use-package elpy :ensure t)
(use-package jedi :ensure t)
(use-package flycheck :ensure t)
(use-package yasnippet :ensure t)
(use-package smartrep :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package counsel :ensure t)
(use-package ivy :demand
  :config
  (setq ivy-wrap t
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
;        ivy-height 20
;        ivy-extra-directories nil
        ivy-count-format "%d/%d "
;        ivy-re-builders-alist '((t . ivy--regex-plus))
        ))
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package go-mode :ensure t)
(use-package company-go :ensure t)

;; - Java minor mode - Meghanada related
(use-package smartparens :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package highlight-symbol :ensure t)
(use-package autodisass-java-bytecode
  :ensure t
  :defer t)
(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))
(use-package meghanada
  ;; :after google-c-style smartparens rainbow-delimiters highlight-symbol
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
	      )
            )

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . counsel-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
  "
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: counsel-git
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" counsel-git)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))

;; - Groovy/Gradle related
(use-package groovy-mode
  :ensure t
  :mode("\.groovy$" "\.gradle$")
  :interpreter("gradle" "groovy")
  :config
  (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
  (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

  ;; Some keys for
  (add-hook 'groovy-mode-hook
            '(lambda ()
               (inf-groovy-keys))))
(use-package groovy-imports :ensure t)
(use-package flycheck-gradle
  :ensure t
  :defer t)

;; - Slack client configure
(use-package slack
  :ensure t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name ""
   :default t
   :client-id ""
   :client-secret ""
   :token ""
   :subscribed-channels '()
   :full-and-display-names t))
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; - Reddit client configure
(use-package md4rd
  :ensure t
  :commands (md4rd)
  :config
  (setq md4rd-subs-active '(emacs hackintosh))
  )
;; minor
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Ivy / Swiper / Counsel related setting
(ivy-mode 1)

;; -- Ivy-based interface to statndard commands
;; --- enable Swiper for i-search alternative
(global-set-key (kbd "C-s") 'swiper)

;; --- enable Swiper search with line-number
(defvar swiper-include-number-in-search t)

;; --- Counsel settings
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; -- Ivy-base interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

;; -- magit related
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

;; -- projectile related
(setq projectile-completion-system 'ivy)

;; - END_OF Ivy/Swiper/Counsel related setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Org-mode and Plugins related settings
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; -- PlantUML related
(autoload 'plantuml-mode "platuml-mode" "PlantUML mode" t)
;; --- path to platuml.jar and options, key-map
(setq plantuml-jar-path "~/.emacs.d/lib/plantuml.jar")
(setq org-plantuml-jar-path "~/.emacs.d/lib/plantuml.jar")

(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")

(setq plantuml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'plantuml-execute)
        map))

;; -- register plantuml to org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml .t)))

;; -- file extensions map
(add-to-list 'auto-mode-alist '("\.pu$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))

;; -- plain plantuml execution
(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    (message "done")))

;; - END_OF Org-mode related setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Company related settings
(global-company-mode)                   ; Enable company in any buffer
(setq company-transformers '(company-sort-by-backend-importance))

(setq company-idle-delay 0)             ; Default: 0.5
(setq company-minimum-prefix-length 5)  ; Default: 4
(setq company-selection-wrap-around t)  ; Goes to first element after laste element
;(setq company-dabbrev-downcase nil)     ; NEED-TO-KNOW-WHAT-THIS-IS

(setq completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yasnippet
(yas-global-mode t)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Frame / Window settings --- Frame >= Window
(display-time-mode +1)
(line-number-mode +1)
(column-number-mode +1)
;; -- in case, which is most of the time,
;; -- init.el is loaded first starting daemon process
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

;; -- Moving between windows in frame
(windmove-default-keybindings)
;; -- Shrink / Enlarge window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)

;; - END_OF Frame / Window settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Theme related
(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))
(unless (package-installed-p 'anti-zenburn-theme)
  (package-install 'anti-zenburn-theme))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Mode-Line related : doom-mode-line for now
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
;;(setq doom-modeline-major-mode-color-icon nil)

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
(add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono 12"))
(set-face-attribute 'default nil :font "BitStream Vera Sans Mono 12")
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Meiryo" :size 14))
(set-fontset-font t 'katakana-jisx0201 (font-spec :family "Meiryo" :size 14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Projectile related settings
(if (eq system-type 'windows-nt)
    (setq projectile-project-search-path '("~/../../Workspace"))
  (setq projectile-project-search-path '("~/Workspace")))

(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test"
                                  :run "npm start"
                                  :test-suffix ".spec")

;; - END_OF Projectile related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indent without tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; - smart-tabs-mode related
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'python)
(add-hook 'js-mode-hook
          (lambda ()
            (smart-tabs-mode-enable)
            (smart-tambs-advice js-indent-line js-indent-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Java-mode related

;; - END_OF Java related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Python-mode related
(setq python-shell-interpreter "python3")

(elpy-enable)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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
            (setq-local electric-indent-mode nil)
            (setq electric-indent-chars (delq ?: electric-indent-chars))))

;; -- smart-tabs-mode hooked
(add-hook 'python-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice python-indent-line-1 python-indent)

;; -- Jedi setup
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

;; - END_OF Python related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - GoLang-mode related
;; -- Go compiler path
;(add-to-list 'exec-path (expand-file-name "xxx"))

;; -- Tools from `go get` executable path
(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path (expand-file-name "~/../../go/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.go/bin")))

(add-hook 'go-mode-hook 'campany-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (setq indent-tabs-mode nil) ; not to user TAB for indent
                          (setq c-basic-offset 4)     ; set tab-size to 4
                          (setq tab-width 4)))        ; set tab0size to 4
                          
;; - END_OF GoLang related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - YAML-mode related
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; - PostgreSQL related
;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (toggle-truncate-lines t)))
;; (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
;; (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")

;; (defun psql-connect (product connection)
;;   (setq sql-product product)
;;   (sql-connect connection))

;; (setq sql-connection-alist
;;       '((psql-nbk-postgres (sql-product 'postgres)
;;                            (sql-port 5432)
;;                            (sql-server "10.143.96.13")
;;                            (sql-user "postgres")
;;                            (sql-password "postgres")
;;                            (sql-database "postgres"))
;;         (psql-nbk-dwhtest (sql-product 'postgres)
;;                           (sql-port 5432)
;;                           (sql-server "10.143.96.13")
;;                           (sql-user "dwhtest")
;;                           (sql-password "dwhtest")
;;                           (sql-database "postgres"))
;;         (psql-nbk-perftest (sql-product 'postgres)
;;                            (sql-port 5432)
;;                            (sql-server "10.143.96.12")
;;                            (sql-user "postgres")
;;                            (sql-password "postgres")
;;                            (sql-database "postgres"))))

;; (defun psql-nbk-postgres-connect ()
;;   (interactive)
;;   (psql-connect 'postgres 'psql-nbk-postgres))
;; (defun psql-nbk-dwhtest-connect ()
;;   (interactive)
;;   (psql-connect 'postgres 'psql-nbk-dwhtest))
;; (defun psql-nbk-perftest-connect ()
;;   (interactive)
;;   (psql-connect 'postgres 'psql-nbk-perftest))

;; - Shell related
(if (eq system-type 'windows-nt)
    (progn (setq explicit-shell-file-name "~/../../local/Git/git-bash.exe")
           (setq explicit-bash.exe-args '("--login" "-i"))
           (setq shell-file-name explicit-shell-file-name)
           (setenv "SHELL" shell-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- auto-generated lines below ---
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
    (md4rd slack flycheck-gradle groovy-imports groovy-mode dap-mode lsp-java lsp-ui company-lsp lsp-mode company-go go-mode projectile anti-zenburn-them smart-tabs-mode python-outline smartrep py-autopep8 flycheck company-jedi jedi yaml-mode evil 2048-game evil-mode magit flymake-python-pyflakes elpy use-package anti-zenburn-theme zenburn-theme company-statistics)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
