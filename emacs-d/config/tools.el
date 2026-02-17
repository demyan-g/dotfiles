;;; tools.el --- External tools configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; This module configures external tools and integrations:
;;
;; - Git integration with Magit
;; - Project management with Projectile
;; - File format support (YAML, JSON, Markdown, etc.)
;; - Docker integration
;; - Terminal emulation with vterm
;;
;; Package manager choices:
;; - JavaScript/TypeScript: pnpm (disk efficiency, strict deps)
;; - Python: uv (Astral ecosystem, 10-100x faster than pip)

;;; Code:

;;; Magit - Git Integration
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g s" . magit-status)
         ("C-c g f" . magit-file-dispatch)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g d" . magit-diff-buffer-file))
  :config
  (setq magit-completing-read-function #'magit-builtin-completing-read)
  (setq magit-refresh-status-buffer nil
        magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-body nil
        magit-diff-refine-hunk nil)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers 'dontask
        magit-repository-directories '(("~/Workspace" . 2))
        magit-clone-default-directory "~/Workspace/")

  (use-package forge
    :ensure t
    :after magit
    :config
    (setq forge-pull-notifications t))

  (when (executable-find "delta")
    (add-hook 'magit-mode-hook
              (lambda () (setenv "GIT_PAGER" "delta")))))

;;; Projectile - Project Management
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c f p" . projectile-find-file)
         ("C-c f r" . projectile-recentf)
         ("C-c f d" . projectile-find-dir)
         ("C-c f g" . projectile-grep))
  :config
  (setq projectile-completion-system 'default
        projectile-project-search-path '("~/Workspace")
        projectile-auto-discover t
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil)
  
  (setq projectile-globally-ignored-directories
        (append '(".git" ".svn" ".hg"
                  ;; Python
                  ".venv" ".tox" ".pytest_cache" "__pycache__" ".mypy_cache" ".ruff_cache"
                  ;; JavaScript/TypeScript
                  "node_modules" ".pnpm-store" ".npm"
                  ;; Rust
                  "target" "dist" "build"
                  ;; Other
                  ".stack-work" ".cask")
                projectile-globally-ignored-directories))
  
  ;;; Project type registrations
  ;; JavaScript/TypeScript with pnpm
  (projectile-register-project-type
   'pnpm '("pnpm-lock.yaml")
   :project-file "package.json"
   :compile "pnpm install"
   :test "pnpm test"
   :run "pnpm dev"
   :test-suffix ".spec")
  
  ;; Fallback for npm projects
  (projectile-register-project-type
   'npm '("package-lock.json")
   :project-file "package.json"
   :compile "npm install"
   :test "npm test"
   :run "npm run dev"
   :test-suffix ".spec")

  ;; Yarn projects
  (projectile-register-project-type
   'yarn '("yarn.lock")
   :project-file "package.json"
   :compile "yarn install"
   :test "yarn test"
   :run "yarn dev"
   :test-suffix ".spec")

  ;; Python with uv
  (projectile-register-project-type
   'python-uv '("uv.lock")
   :project-file "pyproject.toml"
   :compile "uv sync"
   :test "uv run pytest"
   :run "uv run python main.py"
   :test-suffix "_test")
  
  ;; Python with pyproject.toml
  (projectile-register-project-type
   'python-modern '("pyproject.toml")
   :project-file "pyproject.toml"
   :compile "uv sync || pip install -e ."
   :test "uv run pytest || pytest"
   :run "uv run python main.py || python main.py"
   :test-suffix "_test")

  ;; Legacy Python with requirements.txt
  (projectile-register-project-type
   'python-pip '("requirements.txt")
   :project-file "requirements.txt"
   :compile "uv pip install -r requirements.txt"
   :test "pytest"
   :run "python main.py"
   :test-suffix "_test")

  ;; Rust with Cargo
  (projectile-register-project-type
   'rust-cargo '("Cargo.toml")
   :compile "cargo build"
   :test "cargo test"
   :run "cargo run"
   :test-suffix "_test")
  
  ;; C/C++ with CMake
  (projectile-register-project-type
   'cmake '("CMakeLists.txt")
   :project-file "CMakeLists.txt"
   :configure "cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
   :compile "cmake --build build"
   :test "ctest --test-dir build"
   :run "./build/main")

  ;; C/C++ with Meson
  (projectile-register-project-type
   'meson '("meson.build")
   :project-file "meson.build"
   :configure "meson setup builddir"
   :compile "meson compile -C builddir"
   :test "meson test -C builddir"
   :run "./builddir/main")

  ;; Integration with consult
  (use-package consult-projectile
    :ensure t
    :bind (("C-c p f" . consult-projectile-find-file)
           ("C-c p p" . consult-projectile-switch-project)
           ("C-c p b" . consult-projectile-switch-to-buffer)
           ("C-c p r" . consult-projectile-recentf))))

;;; File Format Support
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook ((yaml-mode . display-line-numbers-mode))
  :config
  (setq yaml-indent-offset 2))

(use-package json-mode
  :ensure t
  :mode ("\\.jsonl?\\'" . json-mode)
  :hook (json-mode . (lambda ()
                       (make-local-variable 'js-indent-level)
                       (setq js-indent-level 2))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"
        markdown-asymmetric-header t
        markdown-header-scaling t))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;;; Docker
(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker)
  :config
  (setq docker-image-run-arguments '("-i" "-t" "--rm")))

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.*\\.yml\\'")

;;; Terminal
(use-package vterm
  :ensure t
  :bind (("C-c t t" . vterm)
         ("C-c t o" . vterm-other-window))
  :config
  (setq vterm-shell (executable-find "zsh")
        vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit t)
  (setq vterm-timer-delay 0.01))

;;; Dired Enhancements
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

;;; Apple Music.app control
(defun my/apple-music-command (command)
  "Send a command to Apple Music asynchronously."
  (start-process "apple-music-cmd" nil
                 "osascript" "-e"
                 (format "tell application \"Music\" to %s" command)))

(defun my/music-play-pause ()
  (interactive)
  (my/apple-music-command "playpause"))

(defun my/music-next ()
  (interactive)
  (my/apple-music-command "next track"))

(defun my/music-prev ()
  (interactive)
  (my/apple-music-command "previous track"))

;; Optional: Bind to keys (e.g., Hyper-m or a leader key)
;; (global-set-key (kbd "M-m p") 'my/music-play-pause)
;; (global-set-key (kbd "M-m n") 'my/music-next)
;; (global-set-key (kbd "M-m b") 'my/music-prev)

(provide 'tools)
;;; tools.el ends here
