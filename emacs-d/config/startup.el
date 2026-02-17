;;; startup.el --- startup page configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Startup pageconfiguration:
;; - with Enlight package

;;; Code:

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el"))

(use-package enlight
  :ensure t
  :after grid
  :config
  (setopt initial-buffer-choice #'enlight)
  (setopt enlight-content (enlight--generate-content)))

(defun enlight--generate-content ()
  "Generate the enlight startup content."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (greeting (cond ((< hour 6)  "Good Night")
                         ((< hour 12) "Good Morning")
                         ((< hour 18) "Good Afternoon")
                         (t           "Good Evening"))))
    (concat
     "\n\n"
     (propertize (format "     %s, Demyan     " greeting)
                 'face '(:height 1.9 :weight bold :underline t))
     "\n"
     (propertize (format "%45s" (format-time-string "%Y-%m-%d, %a"))
                 'face 'font-lock-comment-face)
     "\n\n\n"
     (grid-make-row
      `(,(enlight-menu
          '(("  Files & Projects"
             ("  Find File" find-file "f")
             ("  Recent Files" consult-recent-file "r")
             ("  Projects" projectile-switch-project "p")
             ("  Project Files" projectile-find-file "P"))
            ("  Org Mode"
             ("  Agenda" org-agenda "a")
             ("  Capture" org-capture "c")
             ("  Org Directory" (dired "~/Documents/_org") "o"))
            ("  Org Roam"
             ("  Find Node" org-roam-node-find "n")
             ("  Today's Daily" org-roam-dailies-goto-today "d")
             ("  Roam UI" org-roam-ui-mode "g"))))
        "            "
        ,(enlight-menu
          '(("Downloads"
             ("  Downloads folder" (dired "~/Downloads") "D"))
            ("Tools"
             ("  Magit Status" magit-status "m")
             ("  Terminal" vterm "t")
             ("  AI Chat" gptel "i"))
            ("Config"
             ("  Edit Init" (find-file user-init-file) "e")
             ("  Edit Startup" (find-file "~/.emacs.d/config/startup.el") "s"))))))
     "\n\n\n"
     (enlight--recent-files))))

(defun enlight--recent-files ()
  "Generate recent files section."
  (require 'recentf)
  (unless recentf-mode (recentf-mode 1))
  (let* ((recent (seq-take
                  (seq-filter
                   (lambda (f)
                     (and (not (string-match-p "elpaca\\|elpa\\|\\.cache\\|/tmp" f))
                          (file-exists-p f)))
                   recentf-list)
                  8)))
    (if recent
        (concat
         (propertize "  Recent Files  " 'face '(:weight bold :underline t))
         "\n\n"
         (mapconcat
          (lambda (file)
            (format "  %-20s\t    %s"
                    (propertize (file-name-nondirectory file)
                                'face 'font-lock-keyword-face)
                    (propertize (abbreviate-file-name (file-name-directory file))
                                'face 'font-lock-comment-face)))
          recent
          "\n\n"))
      "")))

;;; Keybinding to open enlight manually
(global-set-key (kbd "C-c h") #'enlight-open)

(provide 'startup)
;;; startup.el ends here
