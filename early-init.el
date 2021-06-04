
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
