;;; org-roam-config.el --- org-roam Zettelkasten configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; org-roam v2 for Zettelkasten-style knowledge management:
;; - Node-based note organization
;; - org-roam-ui for graph visualization
;; - consult-org-roam for Vertico integration
;; - citar-org-roam for bibliography management

;;; Code:

;;; org-roam - Core
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/_org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  
  ;; Capture templates for Zettelkasten workflow
  (org-roam-capture-templates
   '(;; Main permanent notes
     ("m" "main" plain "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :draft:\n\n")
      :immediate-finish t
      :unnarrowed t)
     
     ;; Reference/literature notes
     ("r" "reference" plain "%?"
      :if-new (file+head "reference/${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n\n* Source\n\n* Summary\n\n* Notes\n\n")
      :immediate-finish t
      :unnarrowed t)
     
     ;; Fleeting notes (quick captures)
     ("f" "fleeting" plain "%?"
      :if-new (file+head "fleeting/${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :fleeting:\n\n")
      :immediate-finish t
      :unnarrowed t)
     
     ;; Project notes
     ("p" "project" plain "%?"
      :if-new (file+head "projects/${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n* Goals\n\n* Tasks\n\n* Notes\n\n")
      :immediate-finish t
      :unnarrowed t)))
  
  ;; Dailies configuration
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))
     ("t" "task" entry "* TODO %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))
     ("j" "journal" entry "* %<%H:%M> Journal\n%?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n"))))
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
         ("C-c n a" . org-roam-alias-add)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  
  :config
  (require 'org-roam-dailies)
  
  ;; Create directory structure
  (dolist (dir '("main" "reference" "fleeting" "projects" "daily"))
    (let ((path (expand-file-name dir org-roam-directory)))
      (unless (file-exists-p path)
        (make-directory path t))))
  
  ;; Enable database auto-sync
  (org-roam-db-autosync-mode)
  
  ;; Display node in side buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-delete-other-windows . t))))))

;;; org-roam-ui - Graph Visualization
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :bind ("C-c n u" . org-roam-ui-mode))

;;; consult-org-roam - Vertico Integration
(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :bind
  ("C-c n s" . consult-org-roam-search)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n F" . consult-org-roam-forward-links))

;;; citar-org-roam - Bibliography Integration
(use-package citar
  :ensure t
  :custom
  (org-cite-global-bibliography '("~/Documents/_org/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind
  (:map org-mode-map
        ("C-c b" . org-cite-insert)))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}"
        citar-org-roam-capture-template-key "r"))

;;; Helper Functions

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Insert a link to an org-roam node, creating it if needed."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates
         '(("d" "default" plain "%?"
            :if-new (file+head "${slug}.org"
                               "#+title: ${title}\n")
            :immediate-finish t))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  "Filter org-roam nodes by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "List all notes with TAG-NAME."
  (interactive "sTag: ")
  (mapcar #'org-roam-node-file
          (seq-filter (my/org-roam-filter-by-tag tag-name)
                      (org-roam-node-list))))

(defun my/org-roam-find-project ()
  "Find an org-roam node with :project: tag."
  (interactive)
  (org-roam-node-find nil nil (my/org-roam-filter-by-tag "project") nil
                      :templates '(("p" "project" plain "%?"
                                    :if-new (file+head "projects/${slug}.org"
                                                       "#+title: ${title}\n#+filetags: :project:\n")
                                    :immediate-finish t))))

(defun my/org-roam-refresh-agenda-list ()
  "Add org-roam project files to org-agenda-files."
  (interactive)
  (setq org-agenda-files
        (append (list (expand-file-name "gtd.org" org-directory)
                      (expand-file-name "notes.org" org-directory))
                (my/org-roam-list-notes-by-tag "project"))))

;; Refresh agenda files when entering org-roam buffer
(advice-add 'org-agenda :before #'my/org-roam-refresh-agenda-list)

(provide 'org-roam-config)
;;; org-roam-config.el ends here
