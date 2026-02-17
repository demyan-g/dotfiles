;;; musica.el --- Control Apple Music from Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Control macOS Music.app using pytunes CLI.
;; Adapted from https://xenodium.com/emacs-searchplay-music-macos/
;; Converted from Ivy to Vertico/Consult.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)

(defgroup musica nil
  "Control Apple Music from Emacs."
  :group 'multimedia)

(defcustom musica-cache-file
  (expand-file-name ".musica-cache.el" my/cache-dir)
  "File to cache music index."
  :type 'file)

;;; Index Management

(defun musica--index ()
  "Load cached music index."
  (when (file-exists-p musica-cache-file)
    (with-temp-buffer
      (insert-file-contents musica-cache-file)
      (read (current-buffer)))))

(defun musica-index ()
  "Index Music's tracks in background.
Generates index using pytunes and ffprobe."
  (interactive)
  (unless (executable-find "pytunes")
    (user-error "pytunes not installed. Run: uv tool install pytunes"))
  (unless (executable-find "ffprobe")
    (user-error "ffprobe not installed. Run: brew install ffmpeg"))
  (message "Indexing music... started (this runs in background)")
  (let* ((now (current-time))
         (name "Music indexing")
         (buffer (get-buffer-create (format "*%s*" name)))
         (pytunes-path (executable-find "pytunes")))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-sentinel
     (start-process name buffer
                    (file-truename (expand-file-name invocation-name
                                                     invocation-directory))
                    "--quick" "--batch" "--eval"
                    (prin1-to-string
                     `(progn
                        (interactive)
                        (require 'cl-lib)
                        (require 'seq)
                        (require 'map)

                        (add-to-list 'exec-path "/Users/demyan/.local/bin")
                        (setq pytunes-path (executable-find "pytunes"))
                        
                        (message "Generating Tracks.sqlite...")
                        (process-lines pytunes-path "update-index")
                        (message "Generating Tracks.sqlite... done")

                        (defun parse-tags (path)
                          (with-temp-buffer
                            (if (eq 0 (call-process "ffprobe" nil t nil "-v" "quiet"
                                                    "-print_format" "json" "-show_format" path))
                                (map-elt (json-parse-string (buffer-string)
                                                            :object-type 'alist)
                                         'format)
                              (message "Warning: Couldn't read metadata for %s" path)
                              (list (cons 'filename path)))))
                        (let* ((db-path (concat (expand-file-name "~/")
                                                "Music/Music/Music Library.musiclibrary/Tracks.sqlite"))
                               (paths (process-lines "sqlite3" db-path
                                                     "select path from tracks"))
                               (total (length paths))
                               (n 0)
                               (records (seq-map
                                         (lambda (path)
                                           (let ((tags (parse-tags path)))
                                             (message "%d/%d %s" (cl-incf n) total
                                                      (or (map-elt (map-elt tags 'tags) 'title) "No title"))
                                             tags))
                                         paths)))
                          (with-temp-buffer
                            (prin1 records (current-buffer))
                            (write-file ,musica-cache-file nil))))))
     (lambda (process state)
       (if (zerop (process-exit-status process))
           (message "Indexing music... finished (%.1fs)"
                    (float-time (time-subtract (current-time) now)))
         (message "Indexing music... failed, see *%s*" name))))))

;;; Search with Vertico/Consult

(defun musica--format-track (track)
  "Format TRACK for display in completion."
  (let-alist track
    (let ((title (or .tags.title
                     (file-name-base (or .filename ""))
                     "No title"))
          (artist (or .tags.artist ""))
          (album (or .tags.album "")))
      (format "%-40s  %-25s  %s"
              (truncate-string-to-width title 40 nil ?\s "…")
              (propertize (truncate-string-to-width artist 25 nil ?\s "…")
                          'face 'font-lock-keyword-face)
              (propertize (truncate-string-to-width album 30 nil ?\s "…")
                          'face 'font-lock-comment-face)))))

(defun musica-search ()
  "Search and play music using completing-read (Vertico)."
  (interactive)
  (unless (executable-find "pytunes")
    (user-error "pytunes not installed"))
  (let* ((index (musica--index))
         (candidates (mapcar (lambda (track)
                               (cons (musica--format-track track) track))
                             index))
         (selection (completing-read "Play: " candidates nil t)))
    (when-let ((track (cdr (assoc selection candidates))))
      (let-alist track
        (process-lines "pytunes" "play" .filename)
        (message "Playing: %s [%s] %s"
                 (or .tags.title "No title")
                 (or .tags.artist "No artist")
                 (or .tags.album "No album"))))))

;;; Playback Controls

(defun musica-play-pause ()
  "Toggle play/pause."
  (interactive)
  (process-lines "pytunes" "play")
  (musica-info))

(defun musica-next ()
  "Play next track."
  (interactive)
  (process-lines "pytunes" "next")
  (run-at-time 0.5 nil #'musica-info))

(defun musica-previous ()
  "Play previous track."
  (interactive)
  (process-lines "pytunes" "previous")
  (run-at-time 0.5 nil #'musica-info))

(defun musica-random ()
  "Play random track from library."
  (interactive)
  (when-let ((index (musica--index)))
    (let-alist (seq-random-elt index)
      (process-lines "pytunes" "shuffle" "enable")
      (process-lines "pytunes" "play" .filename)
      (musica-info))))

(defun musica-info ()
  "Display current track info."
  (interactive)
  (let ((raw (ignore-errors (process-lines "pytunes" "info"))))
    (if raw
        (message "%s [%s] %s"
                 (string-trim (string-remove-prefix "Title" (nth 3 raw)))
                 (string-trim (string-remove-prefix "Artist" (nth 1 raw)))
                 (string-trim (string-remove-prefix "Album" (nth 2 raw))))
      (message "No track playing"))))

;;; Keybindings

(defvar musica-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'musica-play-pause)
    (define-key map (kbd "s") #'musica-search)
    (define-key map (kbd "i") #'musica-info)
    (define-key map (kbd "n") #'musica-next)
    (define-key map (kbd "p") #'musica-previous)
    (define-key map (kbd "r") #'musica-random)
    (define-key map (kbd "I") #'musica-index)
    map)
  "Keymap for musica commands.")

(global-set-key (kbd "C-c m") musica-command-map)

(provide 'musica)
;;; musica.el ends here
