;;; intelligence.el --- AI integration configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; This module configures AI assistant integration:
;; - gptel: Multi-provider AI chat (ChatGPT, Claude, Gemini)
;; - claude-code: Agentic coding assistance
;;
;; API keys stored in ~/.authinfo.gpg

;;; Code:

;;; gptel - Multi-provider AI Chat
(use-package gptel
  :ensure t
  :bind (("C-c a c" . gptel)            ; Open chat buffer
         ("C-c a s" . gptel-send)       ; Send region/buffer
         ("C-c a m" . gptel-menu)       ; Quick menu
         ("C-c a r" . gptel-rewrite))   ; Rewrite region
  :config
  ;; Default to Claude
  (setq gptel-model "claude-opus-4-5-20251101"
        gptel-default-mode 'org-mode)

  ;; Claude backend (default)
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'gptel-api-key-from-auth-source))

  ;; OpenAI backend
  ;; (gptel-make-openai "ChatGPT"
  ;;   :stream t
  ;;   :key #'gptel-api-key-from-auth-source
  ;;   :models '("gpt-4o" "gpt-4-turbo" "gpt-3.5-turbo"))

  ;; Gemini backend
  (gptel-make-gemini "Gemini"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '("gemini-pro-latest" "gemini-3-pro-preview" "gemini-2.5-pro"))

  ;; Custom directives
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (programmer . "You are an expert programmer. Provide concise, correct code with brief explanations.")
          (writer . "You are a skilled writer. Help improve clarity and style.")
          (translator . "You are a translator fluent in English, Japanese, and Korean. Translate accurately while preserving meaning and nuance."))))

;;; API Key Helper
(defun gptel-api-key-from-auth-source ()
  "Retrieve API key from auth-source based on current backend."
  (let* ((backend gptel-backend)
         (host (cond
                ((string-match-p "openai" (gptel-backend-name backend))
                 "api.openai.com")
                ((string-match-p "anthropic\\|claude" (gptel-backend-name backend))
                 "api.anthropic.com")
                ((string-match-p "gemini" (gptel-backend-name backend))
                 "generativelanguage.googleapis.com")
                (t (error "Unknown backend: %s" (gptel-backend-name backend)))))
         (found (car (auth-source-search :host host
                                         :user "apikey"
                                         :require '(:secret)))))
    (if found
        (funcall (plist-get found :secret))
      (error "No API key found for %s in ~/.authinfo.gpg" host))))

;;; Utility Functions
;; Helper functions for common AI-assisted tasks
(defun ai-improve-writing ()
  "Improve the writing in the selected region or buffer."
  (interactive)
  (gptel-rewrite nil "Improve the clarity and style of this text while preserving its meaning."))

(defun ai-write-commit-message ()
  "Generate a commit message based on staged changes."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (if (string-empty-p diff)
        (message "No staged changes")
      (gptel nil (format "Write a clear, concise commit message for these changes:\n\n%s" diff)))))

(defun ai-explain-code ()
  "Explain the selected code or function at point."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (gptel nil (format "Explain this code in detail:\n\n```\n%s\n```" code))
      (message "No code selected"))))

(defun ai-suggest-improvements ()
  "Suggest improvements for the selected code."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (gptel nil (format "Suggest improvements for this code (efficiency, readability, best practices):\n\n```\n%s\n```" code))
      (message "No code selected"))))

(defun ai-translate (target-lang)
  "Translate selected text to TARGET-LANG."
  (interactive "sTranslate to (en/ja/ko): ")
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "Text to translate: "))))
    (gptel nil (format "Translate the following to %s:\n\n%s"
                       (pcase target-lang
                         ("en" "English")
                         ("ja" "Japanese")
                         ("ko" "Korean")
                         (_ target-lang))
                       text))))

;;; Integration with Other Modes
;; Mode-specific AI configurations

;; Org-mode integration
(with-eval-after-load 'org
  (defun org-ai-summarize-subtree ()
    "Summarize the current org subtree."
    (interactive)
    (let ((content (org-get-subtree)))
      (gptel nil (format "Summarize this content concisely:\n\n%s" content)))))

;; Programming mode integration
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c a e") #'ai-explain-code)
            (local-set-key (kbd "C-c a i") #'ai-suggest-improvements)))

(provide 'intelligence)
;;; intelligence.el ends here
