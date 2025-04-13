;; init.el --- Emacs initial configuration file

;; package --- Summary
;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; Code:

;; no backup files
(setq make-backup-files nil)

;; native-comp related
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/Cellar/gcc/14.2.0_1/lib/gcc/current"
	   "/opt/homebrew/Cellar/gcc/14.2.0_1/lib/gcc/current/gcc/aarch64-apple-darwin24/14"
	   "/opt/homebrew/Cellar/libgccjit/14.2.0_1/lib/gcc/current/")
         ":"))
(setq comp-speed 3
      comp-deferred-compilation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leaf-package related settings
;; (setq gnutls-algorithm-priority "NORMAL: -VERS-TLS1.3")
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init

    ;; optional packages
    (leaf hydra :ensure t) ;; key-bindings related
    ;; (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; DONE installing leaf package

;; other leaf-related packages
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imunu-list-position . 'left))
    )
  )
(leaf macrostep
  :ensure t
  :bind (("C-c e" . mactrostep-expand))
  )
;; - Leaf-related setting END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure <use-package> is avaiable
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; - Ensure packages installed
(eval-when-compile
  (require 'use-package))

(leaf paradox
  :doc "A modern Packages Menu. Colored, with package ratings, and customizable."
  :req "emacs-24.4" "seq-1.7" "let-alist-1.0.3" "spinner-1.7.3" "hydra-0.13.2"
  :tag "packages" "package" "emacs>=24.4"
  :url "https://github.com/Malabarba/paradox"
  :added "2022-04-21"
  :emacs>= 24.4
  :ensure t
  :after spinner hydra
  :config
  (paradox-enable)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired related
(use-package nerd-icons :ensure t)

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-alh")

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-Line related : doom-mode-line for now
(leaf all-the-icons
  :if (window-system)
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3"
  :tag "lisp" "convenient" "emacs>=24.3"
  :url "https://github.com/domtronn/all-the-icons.el"
  :added "2022-04-21"
  :emacs>= 24.3
  :ensure t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))
  )
(use-package doom-modeline
  :ensure t
  :after all-the-icons shrink-path
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  ;; (doom-modeline-major-mode-color-icon nil)

  ;; :hook (after-init . doom-modeline-mode)
  ;; :config
  ;; (doom-modeline-mode)
  )
(doom-modeline-mode) ;; temporary code for hook not working above

;; (leaf doom-modeline
;;   :doc "A minimal and modern mode-line"
;;   :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
;;   :tag "mode-line" "faces" "emacs>=25.1"
;;   :url "https://github.com/seagle0128/doom-modeline"
;;   :added "2022-04-21"
;;   :emacs>= 25.1
;;   :ensure t
;;   :after all-the-icons shrink-path
;;   :custom
;;   (doom-modeline-buffer-file-name-style 'truncate-with-project)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   ;; (doom-modeline-major-mode-color-icon nil)

;;   ;; :hook (after-init . doom-modeline-mode)
;;   )
;; (doom-modeline-mode) ;; temporary code for hook not working above

(leaf hide-mode-line
  :doc "minor mode that hides/masks your modeline"
  :req "emacs-24.4"
  :tag "mode-line" "frames" "emacs>=24.4"
  :url "https://github.com/hlissner/emacs-hide-mode-line"
  :added "2022-04-22"
  :emacs>= 24.4
  :ensure t
  :hook
  ((imenu-list-minor-mode) . hide-mode-line-mode)
  )
(setq display-time-24hr-format t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (set-frame-size (selected-frame) 90 57))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories related - dired

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidates - Ivy / Counsel / Swiper
;; (use-package counsel :ensure t)
(leaf counsel
  :doc "Various completion functions using Ivy"
  :req "emacs-24.5" "ivy-0.13.4" "swiper-0.13.4"
  :tag "tools" "matching" "convenience" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2022-04-22"
  :emacs>= 24.5
  :ensure t
  :after ivy swiper
  )
(leaf ivy
  :demand
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2022-04-22"
  :emacs>= 24.5
  ;; :ensure t
  :config
  (setq ivy-wrap t
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
                                        ;        ivy-height 20
                                        ;        ivy-extra-directories nil
        ivy-count-format "%d/%d "
                                        ;        ivy-re-builders-alist '((t . ivy--regex-plus))
        )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package beacon
  :ensure t
  :diminish beacon-mode
  ;; :require t
  :config
  (beacon-mode 1))
(use-package volatile-highlights
  :ensure t
  ;; :require t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

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

;; DEV-related configs
(leaf smartparens
  :ensure t
  :hook (after-init-hook . smartparens-global-strict-mode)
  :require smartparens-config
  :custom ((electric-pair-mode . nil)))

(use-package plantuml-mode :ensure t)
(leaf company
  :ensure t
  :leaf-defer nil
  :blackout company-mode
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-i" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-tooltip-limit . 12)
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence))
           (global-company-mode . t)
           (company-select-wrap-around . t))
  )
(use-package magit :ensure t)
(use-package yaml-mode :ensure t)
(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3)
           (flycheck-indication-mode . 'left-margin))
  :config (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
  (leaf flycheck-inline
    :ensure t
    :hook (flycheck-mode-hook . flycheck-inline-mode)
    )
  )
(use-package yasnippet :ensure t)
(use-package which-key :ensure t :config (which-key-mode))
(use-package smartrep :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package go-mode :ensure t)
(use-package company-go :ensure t)

;; - Java minor mode - Meghanada related
;; (use-package smartparens :ensure t)
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

;; Tree-Sitter
(leaf tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code
  ;; for which it has a parser available
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))
(leaf tree-sitter-langs
  :ensure t
  :after tree-sitter)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc"))
      )

;; Language Server related
(use-package flymake :ensure t)
(leaf lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((java-mode            ; eclipse-jdtls
    scala-mode           ; metals
    jtsx-jsx-mode        ; ts-ls (tsserver wrapper)
    jtsx-tsx-mode        ; ts-ls (tsserver wrapper)
    jtsx-typescript-mode ; ts-ls (tsserver wrapper)
    js-mode              ; ts-ls (tsserver wrapper)
    ;; js-jsx-mode     ; ts-ls (tsserver wrapper)
    ;; typescript-mode ; ts-ls (tsserver wrapper)
    python-mode     ; pyright
    web-mode
    ) . lsp-deferred)
  (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :custom ((lsp-keymap-prefix . "")
           (lsp-log-io . t)
           (lsp-keep-workspace-alive . nil)
           (lsp-document-sync-method . 2)
           (lsp-response-timeout . 5)
           (lsp-enable-file-watchers . nil))
  :config
  ;; (require 'lsp-clients)
  (setq lsp-auto-guess-root t)
  ;; exclude ".venv, .mypy_cache" from watching
  (dolist (dir '(
                 "[/\\\\]\\.venv$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]\\.__pycache__$"
                 ))
    (push dir lsp-file-watch-ignored))
  :init
  (leaf lsp-ui ;; LSP UI tools
    :ensure t
    :after lsp-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable . t)
    ;; (lsp-ui-doc-position 'top) ; top, bottom, or at-point
    (lsp-ui-doc-position 'at-point)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-max-width . 150)
    (lsp-ui-doc-max-height . 30)
    (lsp-ui-doc-use-childframe . t)
    (lsp-ui-doc-use-webkit . t)

    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable . nil)

    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable . nil)
    (lsp-ui-sideline-ignore-duplicate . t)
    (lsp-ui-sideline-show-symbol . t)
    (lsp-ui-sideline-show-hover . t)
    (lsp-ui-sideline-show-diagnostics . nil)
    (lsp-ui-sideline-show-code-actions . nil)

    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable . nil)
    (lsp-ui-imenu-kind-position 'top)

    ;; lsp-ui-peek
    (lsp-ui-peek-enable . t)
    (lsp-ui-peek-peek-height . 20)
    (lsp-ui-peek-list-width . 50)
    (lsp-ui-peek-fontify 'on-demand) ; never, on-demand, or always
    :preface
    (defun demyan/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    ((lsp-ui-mode-map
      ("C-c C-r" . lsp-ui-peek-find-references)
      ("C-c C-j" . lsp-ui-peek-find-definitions)
      ("C-c i"   . lsp-ui-peek-find-implementation)
      ("C-c m"   . lsp-ui-imenu)
      ("C-c s"   . lsp-ui-sideline-mode)
      ("C-c d"   . ladicle/toggle-lsp-ui-doc))
     (lsp-mode-map ("C-c s" . lsp-ui-sideline-mode)
                   ("C-c d" . lsp-ui-doc-mode)))
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    )
  )

;; LSP ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
(use-package lsp-treemacs :ensure t)

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
;; (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
(ivy-mode 1)

;; -- Ivy-based interface to statndard commands
;; --- enable Swiper for i-search alternative
(global-set-key (kbd "C-s") 'swiper)

;; --- enable Swiper search with line-number
(defvar swiper-include-number-in-search t)

;; --- Counsel settings
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
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

;; Theme related
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-use-variable-pitch t)
  (setq zenburn-scale-org-headlines t)
  (setq zenburn-scale-outline-headlines t)
  (load-theme 'zenburn t))
(use-package anti-zenburn-theme :ensure t)
(use-package nord-theme :ensure t)

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
;; - Auto handling tree-sit features
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Java-mode related
(use-package lsp-java
  :ensure t
  :after lsp-deferred
  :config
  (setq lsp-java-server-install-dir "~/.emacs.d/jdt-language-server")
  (setq lsp-java-workspace-dir "~/.workspace/")
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:~/.emacs.d/lib/lombok-jar/lombok.jar"
         "-Xbootclasspath/a:~/.emacs.d/lib/lombok-jar/lombok.jar"))
  (setq lsp-java-format-settings-url "file://~/.emacs.d/var/eclipse-formatter-xml/eclipse-formatter.xml")
  (setq lsp-java-format-settings-profile "MyProfile")
  (setq lsp-java-autobuild-enabled t)
  (setq lsp-java-save-actions-organize-imports t)
  (setq lsp-java-import-gradle-enabled t)
  (setq lsp-java-import-maven-enabled t)

  ;; Enable DAP (Debug Adapter Protocol) for Java
  (use-package dap-mode
    :config
    (dap-mode t)
    (dap-ui-mode t)
    (use-package dap-java :ensure)
    )
  )

;; - END_OF Java related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Python-mode related
;; (elpy-enable)
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

(leaf lsp-pyright
  :ensure t
  :require t
  :after python
  :defvar lsp-pyright-venv-path
  :init
  (defun lsp-pyright-setup-when-pipenv ()
    (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
    (lsp-restart-workspace))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred)))
  ;; (python-mode-hook . lsp)
  )
(leaf py-isort :ensure t)

;(use-package py-autopep8
;  :ensure t
;  :init (progn
;          (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;          (setq py-autopep8-options '("--ignore=E401"))))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
;;             (define-key python-mode-map (kbd "RET") 'newline-and-indent)
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 4)
;;             (setq python-indent-offset 4)
;;             (setq-local electric-indent-mode nil)
;;             (setq electric-indent-chars (delq ?: electric-indent-chars))))

;; -- smart-tabs-mode hooked
(add-hook 'python-mode-hook 'smart-tabs-mode-enable)
;; (smart-tabs-advice python-indent-line-1 python-indent)

;; -- Jedi setup
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq elpy-rpc-backend "jedi")
;; (setq jedi:complete-on-dot t)
;; (when (require 'flycheck nil t)
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;; (define-key elpy-mode-map (kbd "C-c C-v") 'helm-flycheck)
;; (smartrep-define-key elpy-mode-map "C-c"
;;                      '(("C-n" . flycheck-next-error)
;;                        ("C-p" . flycheck-previous-error)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Typescript-mode related
;; (leaf typescript-mode
;;   :ensure t
;;   :mode "\\.tsx?\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2)
;;   )
(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  ;; :custom
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))
    
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))
;; - END_OF Typescript related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - YAML-mode related
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; - Shell related
(if (eq system-type 'windows-nt)
    (progn (setq explicit-shell-file-name "~/../../local/Git/git-bash.exe")
           (setq explicit-bash.exe-args '("--login" "-i"))
           (setq shell-file-name explicit-shell-file-name)
           (setenv "SHELL" shell-file-name)))

;; Daemon related
;; (setq server-socket-dir "~/.emacs.d/server")
;; (server-start)

;; macOS key binding related
(setq mac-option-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- auto-generated lines below ---
;; auto-generated lines
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" default))
 '(desktop-save-mode t)
 '(package-selected-packages
   '(json-mode flycheck-inline dash-functional volatile-highlights md4rd slack flycheck-gradle groovy-imports groovy-mode dap-mode lsp-java lsp-ui company-lsp lsp-mode company-go go-mode projectile anti-zenburn-them smart-tabs-mode python-outline smartrep py-autopep8 flycheck company-jedi jedi yaml-mode evil 2048-game evil-mode magit flymake-python-pyflakes elpy use-package anti-zenburn-theme zenburn-theme company-statistics))
 '(paradox-github-token t)
 '(warning-suppress-types '((comp) (use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
