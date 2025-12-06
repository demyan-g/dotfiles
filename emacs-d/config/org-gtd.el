;;; org-gtd.el --- GTD task management configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; GTD (Getting Things Done) implementation:
;; - org-agenda configuration
;; - org-capture templates
;; - org-super-agenda for grouped views
;; - org-ql for powerful queries
;; - org-habit for habit tracking

;;; Code:

;;; Agenda Files
(with-eval-after-load 'org
  (setq org-agenda-files
        (list (expand-file-name "inbox.org" org-directory)
              (expand-file-name "gtd.org" org-directory)
              (expand-file-name "calendar.org" org-directory)
              (expand-file-name "habits.org" org-directory))))

;;; Capture Templates
(with-eval-after-load 'org
  (setq org-capture-templates
        `(;; Inbox - quick capture
          ("i" "Inbox" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)
          
          ;; Task with scheduled date
          ("t" "Task" entry
           (file+headline ,(expand-file-name "gtd.org" org-directory) "Tasks")
           "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :empty-lines 1)
          
          ;; Task with deadline
          ("d" "Deadline" entry
           (file+headline ,(expand-file-name "gtd.org" org-directory) "Tasks")
           "* TODO %^{Task}\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :empty-lines 1)
          
          ;; Meeting/Appointment
          ("m" "Meeting" entry
           (file+headline ,(expand-file-name "calendar.org" org-directory) "Meetings")
           "* %^{Meeting title}\n%^T\n:PROPERTIES:\n:LOCATION: %^{Location}\n:END:\n\n%?"
           :empty-lines 1)
          
          ;; Habit
          ("h" "Habit" entry
           (file ,(expand-file-name "habits.org" org-directory))
           "* TODO %^{Habit}\nSCHEDULED: %(format-time-string \"%Y-%m-%d %a .+1d/3d\")\n:PROPERTIES:\n:STYLE: habit\n:END:\n"
           :empty-lines 1)
          
          ;; Note
          ("n" "Note" entry
           (file ,(expand-file-name "notes.org" org-directory))
           "* %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
           :empty-lines 1)
          
          ;; Journal
          ("j" "Journal" entry
           (file+datetree ,(expand-file-name "journal.org" org-directory))
           "* %<%H:%M> %^{Title}\n%?"
           :empty-lines 1))))

;;; Refile Configuration
(with-eval-after-load 'org
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

;;; Agenda Configuration
(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-block-separator ?─)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")
  
  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting On")))
            (tags-todo "inbox"
                       ((org-agenda-overriding-header "Inbox")))))
          
          ("n" "Next Actions"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")))))
          
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "DONE"
                  ((org-agenda-overriding-header "Completed This Week")))
            (todo "TODO"
                  ((org-agenda-overriding-header "All TODOs"))))))))

;;; org-super-agenda - Grouped Views
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t
                 :date today
                 :scheduled today
                 :order 1)
          (:name "Due Today"
                 :deadline today
                 :order 2)
          (:name "Overdue"
                 :deadline past
                 :face (:foreground "#ff6b6b")
                 :order 0)
          (:name "Important"
                 :priority "A"
                 :order 3)
          (:name "Next Actions"
                 :todo "NEXT"
                 :order 4)
          (:name "Waiting"
                 :todo "WAITING"
                 :order 6)
          (:name "Projects"
                 :tag "project"
                 :order 5)
          (:name "Habits"
                 :habit t
                 :order 7)
          (:name "Inbox"
                 :tag "inbox"
                 :order 8)
          (:name "Work"
                 :tag "@work"
                 :order 10)
          (:name "Home"
                 :tag "@home"
                 :order 11)
          (:discard (:anything t)))))

;;; org-ql - Powerful Queries
(use-package org-ql
  :ensure t
  :after org
  :config
  (setq org-ql-views
        '(("Stuck Projects"
           :buffers-files org-agenda-files
           :query (and (todo) (tag "project")
                       (not (descendants (todo "NEXT"))))
           :title "Stuck Projects"
           :super-groups ((:auto-property "PROJECT")))
          
          ("Due This Week"
           :buffers-files org-agenda-files
           :query (and (todo)
                       (deadline :to +7))
           :title "Due This Week"
           :sort (deadline))
          
          ("High Priority"
           :buffers-files org-agenda-files
           :query (and (todo) (priority "A"))
           :title "High Priority Tasks"))))

;;; org-habit
(with-eval-after-load 'org
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-show-all-today nil))

;;; org-edna - Task Dependencies
(use-package org-edna
  :ensure t
  :after org
  :config
  (org-edna-mode))

;;; TODO Keyword Faces
(with-eval-after-load 'org
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6b6b" :weight bold))
          ("NEXT" . (:foreground "#4dabf7" :weight bold))
          ("WAITING" . (:foreground "#ffa94d" :weight bold))
          ("HOLD" . (:foreground "#868e96" :weight bold))
          ("DONE" . (:foreground "#51cf66" :weight bold))
          ("CANCELLED" . (:foreground "#495057" :weight bold :strike-through t)))))

;;; Priority Configuration
(with-eval-after-load 'org
  (setq org-priority-faces
        '((?A . (:foreground "#ff6b6b" :weight bold))
          (?B . (:foreground "#ffa94d"))
          (?C . (:foreground "#868e96"))))
  (setq org-default-priority ?B)
  (setq org-lowest-priority ?C))

;;; Clock Configuration
(with-eval-after-load 'org
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

;;; Archive Configuration
(with-eval-after-load 'org
  (setq org-archive-location
        (expand-file-name "archive/%s_archive::" org-directory)))

;;; Helper Functions

(defun my/org-inbox-process ()
  "Process inbox items - refile or schedule."
  (interactive)
  (find-file (expand-file-name "inbox.org" org-directory))
  (goto-char (point-min))
  (org-next-visible-heading 1))

(defun my/org-agenda-process-inbox ()
  "Process inbox from agenda view."
  (interactive)
  (org-agenda-goto)
  (org-narrow-to-subtree)
  (org-show-subtree)
  (org-refile))

;;; Keybindings
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c i") #'my/org-inbox-process)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "r" #'my/org-agenda-process-inbox))

(provide 'org-gtd)
;;; org-gtd.el ends here
