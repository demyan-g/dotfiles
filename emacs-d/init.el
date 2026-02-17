;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Modern, modular Emacs configuration featuring:
;; - Elpaca for async package management
;; - Vertico + Consult + Marginalia + Embark + Corfu completion stack
;; - LSP-mode with language-specific optimizations
;; - org-roam for Zettelkasten knowledge management
;; - Trilingual support (English/Japanese/Korean) with Sarasa Gothic
;; - doom-modeline + nerd-icons for UI

;;; Code:

;;; Elpaca Package Manager Bootstrap
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              ;; :build (:not elpaca--activate-package)))
                              :build (:not elpaca--hierarchical-env-loading)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; use-package Integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)

;; Install transient early (required by magit, gptel, etc.)
(elpaca transient)
(elpaca-wait)

;;; Load Configuration Modules
(defun load-config (name)
  "Load configuration file NAME from config directory."
  (load (expand-file-name (concat "config/" name) user-emacs-directory)))

;; Core functionality
(load-config "core")
(load-config "fonts")

;; UI packages - load and wait
(load-config "appearance")
(load-config "completion")
(elpaca-wait)  ;; Wait for UI packages to install

;; External tools
(load-config "tools")
(elpaca-wait)  ;; Wait for tools (magit, projectile, etc.)

;; Music playing related
(load-config "musica")
(elpaca-wait) ;; Wait for musica

;; Development tools
(load-config "development")
(load-config "lsp-config")
(elpaca-wait)  ;; Wait for dev packages

;; AI integration
(load-config "intelligence")
(elpaca-wait)

;; Language-specific configurations
(load-config "lang-java")
(load-config "lang-scala")
(load-config "lang-python")
(load-config "lang-rust")
(load-config "lang-c")
(load-config "lang-web")
(elpaca-wait)  ;; Wait for language packages

;; Org-mode ecosystem
(load-config "org-config")
(load-config "org-roam-config")
(load-config "org-babel-config")
(load-config "org-export-config")
(load-config "org-gtd")
(elpaca-wait)  ;; Wait for org packages

;; CJK input method support
(load-config "input-method")
(elpaca-wait)  ;; Wait for input-method conf

;; Startup page
(load-config "startup")
(elpaca-wait)  ;; Final wait

;;; Post-Initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Reset GC threshold to reasonable value
            (setq gc-cons-threshold (* 16 1024 1024))
            ;; Display startup time
            (message "Emacs loaded in %.3f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
