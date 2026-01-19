;;; essays.el --- Essay management for Doom Emacs -*- lexical-binding: t; -*-

;; Essay management system that creates individual org files for essays
;; with slugified filenames and metadata templates.

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar my/essays-dir (expand-file-name "essays/" org-directory)
  "Directory for essay files.")

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/essays-ensure-directory ()
  "Ensure the essays directory exists."
  (unless (file-exists-p my/essays-dir)
    (make-directory my/essays-dir t)
    (message "Created essays directory: %s" my/essays-dir)))

(defun my/essays-slugify (title)
  "Convert TITLE to a filename-safe slug.
Converts to lowercase, replaces non-alphanumeric chars with hyphens,
and removes leading/trailing hyphens."
  (let* ((downcase-title (downcase title))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" downcase-title)))
    ;; Remove leading and trailing hyphens
    (setq slug (replace-regexp-in-string "^-+" "" slug))
    (setq slug (replace-regexp-in-string "-+$" "" slug))
    slug))

(defun my/essays-format-date-for-filename ()
  "Return current date formatted for filename (MM-DD-YY)."
  (format-time-string "%m-%d-%y"))

(defun my/essays-format-date-for-header ()
  "Return current date formatted for file header (Month DD, YYYY)."
  (format-time-string "%B %d, %Y"))

;; ============================================================================
;; MAIN ENTRY FUNCTIONS
;; ============================================================================

(defun my/essays-new ()
  "Create a new essay file.
Prompts for a title, creates the file with metadata template,
and positions cursor ready to write."
  (interactive)
  (my/essays-ensure-directory)
  (let* ((title (read-string "Essay title: "))
         (slug (my/essays-slugify title))
         (date-suffix (my/essays-format-date-for-filename))
         (filename (format "%s-%s.org" slug date-suffix))
         (file-path (expand-file-name filename my/essays-dir)))
    (if (file-exists-p file-path)
        (if (y-or-n-p (format "File %s already exists. Open it? " filename))
            (find-file file-path)
          (message "Aborted."))
      ;; Create new file with template
      (find-file file-path)
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (my/essays-format-date-for-header)))
      (insert "#+STARTUP: showall\n\n")
      (message "Created new essay: %s" filename))))

(defun my/essays-list ()
  "List all essays and open the selected one."
  (interactive)
  (my/essays-ensure-directory)
  (let* ((files (directory-files my/essays-dir nil "\\.org$"))
         (essays (cl-remove-if (lambda (f) (string-prefix-p "." f)) files)))
    (if essays
        (let* ((choice (completing-read "Select essay: " (reverse essays) nil t)))
          (find-file (expand-file-name choice my/essays-dir)))
      (message "No essays found in %s" my/essays-dir))))

(defun my/essays-open-directory ()
  "Open the essays directory in dired."
  (interactive)
  (my/essays-ensure-directory)
  (dired my/essays-dir))

(provide 'essays)
;;; essays.el ends here
