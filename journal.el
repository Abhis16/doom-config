;;; journal.el --- Weekly journal system for Doom Emacs -*- lexical-binding: t; -*-

;; Weekly journal system that creates separate org files for each week (Monday-Sunday)
;; with date headings for each day's entries.

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar my/journal-dir (expand-file-name "journal/" org-directory)
  "Directory for weekly journal files.")

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/journal-get-week-start (&optional date)
  "Get the Monday of the week containing DATE (defaults to today).
Returns a time value representing Monday at 00:00:00."
  (let* ((time (or date (current-time)))
         (decoded (decode-time time))
         ;; dow: 0 = Sunday, 1 = Monday, ..., 6 = Saturday
         (dow (nth 6 decoded))
         ;; Days to subtract to get to Monday (Sunday=0 needs 6, Monday=1 needs 0, etc.)
         (days-back (mod (- dow 1) 7)))
    (time-subtract time (days-to-time days-back))))

(defun my/journal-week-file-name (&optional date)
  "Generate the filename for the week containing DATE.
Format: YYYY-MM-DD.org where the date is the Monday of that week."
  (format-time-string "%Y-%m-%d.org" (my/journal-get-week-start date)))

(defun my/journal-week-file-path (&optional date)
  "Return the full path to the week file for DATE."
  (expand-file-name (my/journal-week-file-name date) my/journal-dir))

(defun my/journal-today-heading ()
  "Return today's date formatted as a heading.
Format: 'DayName, Month DD, YYYY' (e.g., 'Thursday, January 16, 2026')."
  (format-time-string "%A, %B %d, %Y"))

(defun my/journal-week-title (&optional date)
  "Return the title for the week containing DATE.
Format: 'Week of Month DD, YYYY' (e.g., 'Week of January 13, 2026')."
  (format-time-string "Week of %B %d, %Y" (my/journal-get-week-start date)))

(defun my/journal-current-time-heading ()
  "Return current time formatted for entry heading (HH:MM)."
  (format-time-string "%H:%M"))

;; ============================================================================
;; FILE CREATION AND NAVIGATION
;; ============================================================================

(defun my/journal-ensure-directory ()
  "Ensure the journal directory exists."
  (unless (file-exists-p my/journal-dir)
    (make-directory my/journal-dir t)
    (message "Created journal directory: %s" my/journal-dir)))

(defun my/journal-create-week-file (file-path &optional date)
  "Create a new week file at FILE-PATH with appropriate header."
  (with-temp-buffer
    (insert (format "#+TITLE: %s\n" (my/journal-week-title date)))
    (insert "#+STARTUP: overview\n\n")
    (write-file file-path)))

(defun my/journal-find-or-create-today-heading ()
  "Find today's heading in the current buffer, creating it if necessary.
Returns the position of the heading."
  (let ((today-heading (my/journal-today-heading))
        (found nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* %s$" (regexp-quote today-heading)) nil t)
          (setq found (line-beginning-position))
        ;; Heading not found, create it at the end
        (goto-char (point-max))
        ;; Ensure we're on a new line
        (unless (bolp) (insert "\n"))
        ;; Add blank line before heading if there's content
        (unless (= (point-min) (point))
          (insert "\n"))
        (insert "* " today-heading "\n")
        (setq found (line-beginning-position 0))))
    found))

(defun my/journal-goto-entry-point ()
  "Go to the position where a new entry should be inserted.
This is at the end of today's section, ready for a new timestamped entry."
  (let ((today-heading (my/journal-today-heading)))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s$" (regexp-quote today-heading)) nil t)
        (progn
          ;; Found today's heading, go to end of section
          ;; (before next level-1 heading or end of file)
          (if (re-search-forward "^\\* " nil t)
              (progn
                (beginning-of-line)
                (forward-line -1)
                ;; Skip back over blank lines
                (while (and (looking-at "^$") (> (point) (point-min)))
                  (forward-line -1))
                (end-of-line))
            ;; No next heading, go to end of buffer
            (goto-char (point-max))
            ;; Remove trailing whitespace
            (while (and (> (point) (point-min))
                        (looking-back "[\n\t ]" (1- (point))))
              (delete-char -1))))
      ;; Today's heading doesn't exist (shouldn't happen if called correctly)
      (goto-char (point-max)))))

;; ============================================================================
;; MAIN ENTRY FUNCTIONS
;; ============================================================================

(defun my/journal-new-entry ()
  "Add a new timestamped journal entry for today.
Opens the current week's file, creates today's heading if needed,
and positions cursor ready to type after a timestamp."
  (interactive)
  (my/journal-ensure-directory)
  (let* ((file-path (my/journal-week-file-path))
         (file-exists (file-exists-p file-path)))
    ;; Create file if it doesn't exist
    (unless file-exists
      (my/journal-create-week-file file-path))
    ;; Open the file
    (find-file file-path)
    ;; Ensure today's heading exists
    (my/journal-find-or-create-today-heading)
    ;; Go to entry point and insert timestamp
    (my/journal-goto-entry-point)
    ;; Insert new entry with timestamp
    (insert "\n\n** " (my/journal-current-time-heading) " ")))

(defun my/journal-open-current-week ()
  "Open the current week's journal file for reading.
Creates the file if it doesn't exist."
  (interactive)
  (my/journal-ensure-directory)
  (let ((file-path (my/journal-week-file-path)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (if (y-or-n-p "No journal for this week yet. Create it? ")
          (progn
            (my/journal-create-week-file file-path)
            (find-file file-path))
        (message "No journal file for this week.")))))

(defun my/journal-read-week ()
  "Open a journal file for any week by selecting a date.
Uses org's date picker to select the date, then opens that week's file."
  (interactive)
  (let* ((selected-date (org-read-date nil t nil "Select a date in the week to view:"))
         (file-path (my/journal-week-file-path selected-date)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "No journal exists for the week of %s"
               (my/journal-week-title selected-date)))))

(defun my/journal-list-weeks ()
  "List all available journal weeks and allow selection."
  (interactive)
  (my/journal-ensure-directory)
  (let* ((files (directory-files my/journal-dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
         (weeks (mapcar (lambda (f)
                          (let* ((date-str (file-name-sans-extension f))
                                 (time (date-to-time (concat date-str " 00:00:00"))))
                            (cons (my/journal-week-title time) f)))
                        (reverse files))))  ; Most recent first
    (if weeks
        (let* ((choice (completing-read "Select week: " (mapcar #'car weeks) nil t))
               (file (cdr (assoc choice weeks))))
          (find-file (expand-file-name file my/journal-dir)))
      (message "No journal files found in %s" my/journal-dir))))

(provide 'journal)
;;; journal.el ends here
