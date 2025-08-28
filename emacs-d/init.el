;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; Author: SeungJun Choi <seungjun.choi@demyan.io>
;; Version: 0.3
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is the main entry point for a modern, modular Emacs configuration.
;; It bootstraps the Elpaca pacakge manager and loads configuration modules
;; in a specific order to ensure proper initialization.
;;
;; The configuration is organized into logical modules:
;; - Core settings (encoding, backups, basic behavior)
;; - UI configurations (themes, modeline, visual enhancements)
;; - Completion system (Vertico, Consult, Marginalia, Embark)
;; - AI integration (gptel, claude-code)
;; - Development tools (LSP, syntax checking, snippets)
;; - Language-specific configurations (Python Java, Scala, etc.)
;; - External tools (Magit, Projectile, etc.)
;; - Org-mode configuration
;;
;; This modular approach makes it easy to:
;; - Find and modify specific configurations
;; - Disable features by commenting out module loads
;; - Add new modules for additional functionality
;; - Track changes in version control

;;; Code:

;;; Elpaca Package Manager Bootstrap
;; Elpaca is a modern package manager that offers several advantages:
;; - Asynchronous, parallel package downloads
;; - Declarative package recipes
;; - Built-in straight.el compatibility
;; - better reproducibility with lockfiles

(defvar elpaca-installer-version 0.8
  "Version of the Elpaca installer.")

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory)
  "Directory where Elpaca stores its files.")

(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory)
  "Directory where Elpaca builds packages.")

(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory)
  "Directory where Elpaca clones package repositories.")

(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files
                              (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package))
  "Recipe for Elpaca itself.")

;; Bootstrap Elpaca
;; This code checks if Elpaca is installed and installs it if necessary
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ;; Clone Elpaca repository
                 ((zerop
                   (apply #'call-process
                          `("git" nil ,buffer t "clone"
                            ,@(when-let ((depth (plist-get order :depth)))
                                (list (format "--depth=%d" depth)
                                      "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
                 ;; Check out the specified refactored
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ;; Byte-compile Elpaca
                 ((zerop
                   (call-process emacs nil buffer nil
                                 "-Q" "-L" "." "--batch" "--eval"
                                 "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

;; Enable Elpaca's queue processing after init
(add-hook 'after-init-hook #'elpaca-process-queues)

;; Install Elpaca itself
(elpaca `(,@elpaca-order))

;;; use-package Integration
;; Enable use-package support for Elpaca
;; This allows to use the familiar use-package syntax
;; while leveraging Elpaca's superior package management
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca
        (elpaca-use-package-mode))

;; Make use-pacakge always ensure packages are installed
(setq use-package-always-ensure t)

;;; Load Path Configuration
;; Add config directory to the load path so that can require modules
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;;; Module Loading
;; Load configuration modules in order
;; Each module is self-contained and can be disabled by commenting out its line

;; Core settings - must be loaded first
(require 'core)

;; UI configuration - themes, modeline, visual enchancements
(require 'apprearance)

;; Modern completion system - Vertico, Consult, etc.
(require 'completion)

;; LLM integration - gptel and claude-code
(require 'llm)

;; General development tools - syntax checking, snippets, etc.
(require 'development)

;; Language Server Protocol configuration
(require 'lsp-config)

;; Language-specific configurations
(require 'lang-java)
(require 'lang-scala)

;; External tools - version control, project management
(require 'project-management)

;; Org-mode configuration
(require 'org-config)

;;; Post-initialization
;; Reset garbage collection threshold to a more reasonable value
;; Use 2MB here as a balance between performance and memory usage
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; End:

(provide 'init)
;;; init.el ends here
