;;; dashboard.el --- Unified dashboard for Doom Emacs -*- lexical-binding: t; -*-

;; A unified view combining org-agenda, habits, and top todos.

;; ============================================================================
;; CORE VARIABLES
;; ============================================================================

(defvar my/dashboard-buffer-name "*Dashboard*"
  "Name of the dashboard buffer.")

;; ============================================================================
;; MODE DEFINITION
;; ============================================================================

(defvar my/dashboard-mode-map
  (make-sparse-keymap)
  "Keymap for dashboard buffer.")

(define-derived-mode my/dashboard-mode special-mode "Dashboard"
  "Major mode for the unified dashboard."
  (setq buffer-read-only t
        truncate-lines t)
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'my/dashboard-mode 'normal)))

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/dashboard-quit ()
  "Quit dashboard and return to doom dashboard."
  (interactive)
  ;; Kill the dashboard buffer
  (when (get-buffer my/dashboard-buffer-name)
    (kill-buffer my/dashboard-buffer-name))
  ;; Kill the org-agenda buffer if open
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*"))
  ;; Return to single window with dashboard
  (delete-other-windows)
  (+doom-dashboard/open (selected-frame)))

(defun my/dashboard-get-line-type ()
  "Get the type of item at point."
  (get-text-property (point) 'line-type))

(defun my/dashboard-get-item-data ()
  "Get the data for item at point."
  (get-text-property (point) 'item-data))

(defun my/dashboard-get-org-files ()
  "Get list of org files from org-agenda-files.
Expands directories to their contained .org files."
  (let ((files '()))
    (dolist (entry org-agenda-files)
      (let ((expanded (expand-file-name entry)))
        (if (file-directory-p expanded)
            ;; It's a directory - get all .org files in it
            (dolist (f (directory-files expanded t "\\.org$"))
              (when (file-regular-p f)
                (push f files)))
          ;; It's a file
          (when (file-regular-p expanded)
            (push expanded files)))))
    (nreverse files)))

;; ============================================================================
;; HABITS SECTION
;; ============================================================================

(defun my/dashboard-render-habits-section ()
  "Render the habits section of the dashboard."
  ;; Ensure habit states are current
  (my/habit-ensure-states-current)

  (let ((daily-habits (my/habit-get-habits-by-type 'daily))
        (weekly-habits (my/habit-get-habits-by-type 'weekly)))

    ;; Daily Habits
    (insert (propertize "DAILY HABITS " 'face 'bold))
    (insert (propertize "(finalize by midnight)\n" 'face 'font-lock-comment-face))
    (insert (propertize (make-string 40 ?─) 'face 'font-lock-comment-face) "\n")
    (if daily-habits
        (dolist (habit daily-habits)
          (let* ((state (my/habit-get-state habit 'daily))
                 (indicator (my/habit-get-status-indicator state))
                 (face (my/habit-get-status-face state))
                 (line (format "%s %s\n" indicator habit)))
            (insert (propertize line
                                'face face
                                'line-type 'habit
                                'item-data (list :name habit :type 'daily)))))
      (insert (propertize "  (no daily habits)\n" 'face 'font-lock-comment-face)))
    (insert "\n")

    ;; Weekly Habits
    (insert (propertize "WEEKLY HABITS " 'face 'bold))
    (insert (propertize "(finalize by Sunday midnight)\n" 'face 'font-lock-comment-face))
    (insert (propertize (make-string 40 ?─) 'face 'font-lock-comment-face) "\n")
    (if weekly-habits
        (dolist (habit weekly-habits)
          (let* ((state (my/habit-get-state habit 'weekly))
                 (indicator (my/habit-get-status-indicator state))
                 (face (my/habit-get-status-face state))
                 (line (format "%s %s\n" indicator habit)))
            (insert (propertize line
                                'face face
                                'line-type 'habit
                                'item-data (list :name habit :type 'weekly)))))
      (insert (propertize "  (no weekly habits)\n" 'face 'font-lock-comment-face)))
    (insert "\n")))

;; ============================================================================
;; AGENDA SECTION
;; ============================================================================

(defun my/dashboard-get-upcoming-deadlines ()
  "Get list of upcoming deadlines from org files.
Returns list of (heading deadline-str file) sorted by deadline."
  (let ((entries '())
        (today (current-time))
        (week-later (time-add (current-time) (days-to-time 7))))
    (dolist (file (my/dashboard-get-org-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ \\(TODO\\|IN-PROGRESS\\|WAITING\\) \\(.+\\)$" nil t)
          (let ((status (match-string 1))
                (heading (match-string 2)))
            ;; Look for DEADLINE in the next few lines
            (save-excursion
              (let ((limit (save-excursion
                             (or (re-search-forward "^\\*" nil t) (point-max)))))
                (when (re-search-forward "DEADLINE: <\\([0-9]+-[0-9]+-[0-9]+\\)" limit t)
                  (let* ((deadline-str (match-string 1))
                         (deadline-time (date-to-time (concat deadline-str " 00:00:00"))))
                    ;; Only include if within next 7 days
                    (when (and (time-less-p today deadline-time)
                               (time-less-p deadline-time week-later))
                      (push (list heading deadline-str status file) entries))))))))))
    ;; Sort by deadline
    (sort entries (lambda (a b)
                    (string< (nth 1 a) (nth 1 b))))))

(defun my/dashboard-render-agenda-section ()
  "Render the upcoming deadlines section."
  (insert (propertize "UPCOMING DEADLINES (7 days)\n" 'face 'bold))
  (insert (propertize (make-string 50 ?─) 'face 'font-lock-comment-face) "\n")
  (let ((deadlines (my/dashboard-get-upcoming-deadlines)))
    (if deadlines
        (dolist (entry deadlines)
          (let* ((heading (nth 0 entry))
                 (deadline (nth 1 entry))
                 (status (nth 2 entry))
                 (line (format "  %s  %s [%s]\n" deadline heading status)))
            (insert (propertize line
                                'line-type 'deadline
                                'item-data entry))))
      (insert (propertize "  (no upcoming deadlines)\n" 'face 'font-lock-comment-face))))
  (insert "\n"))

;; ============================================================================
;; TOP TODOS SECTION
;; ============================================================================

(defun my/dashboard-get-todos-by-tag (tag &optional limit)
  "Get TODOs with TAG, sorted by deadline, limited to LIMIT entries.
Returns list of (heading deadline-str status)."
  (let ((entries '())
        (limit (or limit 5)))
    (dolist (file (my/dashboard-get-org-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (format "^\\*+ \\(TODO\\|IN-PROGRESS\\|WAITING\\) \\(.+\\)[ \t]+:%s:" tag) nil t)
          (let ((status (match-string 1))
                (heading (replace-regexp-in-string "[ \t]+:[a-zA-Z:]+:$" "" (match-string 2))))
            ;; Look for DEADLINE
            (save-excursion
              (let ((limit-pos (save-excursion
                                 (or (re-search-forward "^\\*" nil t) (point-max))))
                    (deadline-str nil))
                (when (re-search-forward "DEADLINE: <\\([0-9]+-[0-9]+-[0-9]+\\)" limit-pos t)
                  (setq deadline-str (match-string 1)))
                (push (list heading deadline-str status) entries)))))))
    ;; Sort by deadline (nil deadlines go last)
    (setq entries (sort entries
                        (lambda (a b)
                          (let ((da (nth 1 a))
                                (db (nth 1 b)))
                            (cond
                             ((and da db) (string< da db))
                             (da t)
                             (t nil))))))
    ;; Limit results
    (seq-take entries limit)))

(defun my/dashboard-render-todos-section ()
  "Render the top 5 todos per category section."
  (insert (propertize "TOP 5 TASKS\n" 'face 'bold))
  (insert (propertize (make-string 50 ?─) 'face 'font-lock-comment-face) "\n")

  (dolist (category '(("personal" . "Personal")
                      ("research" . "Research")
                      ("school" . "School")))
    (let* ((tag (car category))
           (label (cdr category))
           (todos (my/dashboard-get-todos-by-tag tag 5)))
      (insert (propertize (format "  %s:\n" label) 'face 'font-lock-keyword-face))
      (if todos
          (let ((idx 1))
            (dolist (todo todos)
              (let* ((heading (nth 0 todo))
                     (deadline (nth 1 todo))
                     (deadline-display (if deadline
                                           (format " (due: %s)" deadline)
                                         ""))
                     (line (format "    %d. %s%s\n" idx heading deadline-display)))
                (insert (propertize line
                                    'line-type 'todo
                                    'item-data todo))
                (setq idx (1+ idx)))))
        (insert (propertize "    (none)\n" 'face 'font-lock-comment-face)))))
  (insert "\n"))

;; ============================================================================
;; MAIN RENDER FUNCTION
;; ============================================================================

(defun my/dashboard-render ()
  "Render the complete dashboard."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert (propertize
             (format "══════════════════════════════════════════════════\n")
             'face 'font-lock-comment-face))
    (insert (propertize
             (format "          DASHBOARD - %s\n"
                     (format-time-string "%A, %b %d, %Y"))
             'face 'bold))
    (insert (propertize
             (format "══════════════════════════════════════════════════\n\n")
             'face 'font-lock-comment-face))

    ;; Sections
    (my/dashboard-render-agenda-section)
    (my/dashboard-render-habits-section)
    (my/dashboard-render-todos-section)

    ;; Footer
    (insert (propertize
             (format "══════════════════════════════════════════════════\n")
             'face 'font-lock-comment-face))
    (insert (propertize
             "Keys: RET=toggle  c=complete  i=incomplete  x=cancel  r=refresh  q=quit\n"
             'face 'font-lock-comment-face))

    (goto-char (point-min))))

;; ============================================================================
;; INTERACTIVE COMMANDS
;; ============================================================================

(defun my/dashboard-refresh ()
  "Refresh the dashboard (both panes)."
  (interactive)
  ;; Refresh the habits/todos pane
  (when (get-buffer my/dashboard-buffer-name)
    (with-current-buffer my/dashboard-buffer-name
      (my/dashboard-render-right-pane)))
  ;; Refresh org-agenda if open
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)))
  (message "Dashboard refreshed"))

(defun my/dashboard-toggle-at-point ()
  "Toggle item at point (habit) or jump to todo."
  (interactive)
  (let ((line-type (my/dashboard-get-line-type))
        (item-data (my/dashboard-get-item-data)))
    (pcase line-type
      ('habit
       (let ((name (plist-get item-data :name))
             (type (plist-get item-data :type)))
         (my/habit-cycle-state name type)
         (my/dashboard-refresh)))
      ('todo
       (message "Todo: %s" (car item-data)))
      ('deadline
       (message "Deadline: %s" (car item-data)))
      (_ (message "No item at point")))))

(defun my/dashboard-mark-habit (state)
  "Mark habit at point with STATE."
  (let ((line-type (my/dashboard-get-line-type))
        (item-data (my/dashboard-get-item-data)))
    (when (eq line-type 'habit)
      (let ((name (plist-get item-data :name))
            (type (plist-get item-data :type)))
        (my/habit-set-state name type state)
        (my/dashboard-refresh)))))

(defun my/dashboard-mark-completed ()
  "Mark habit at point as completed."
  (interactive)
  (my/dashboard-mark-habit 'completed))

(defun my/dashboard-mark-incomplete ()
  "Mark habit at point as incomplete."
  (interactive)
  (my/dashboard-mark-habit 'incomplete))

(defun my/dashboard-mark-cancelled ()
  "Mark habit at point as cancelled."
  (interactive)
  (my/dashboard-mark-habit 'cancelled))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(map! :map my/dashboard-mode-map
      :n "RET" #'my/dashboard-toggle-at-point
      :n "c" #'my/dashboard-mark-completed
      :n "i" #'my/dashboard-mark-incomplete
      :n "x" #'my/dashboard-mark-cancelled
      :n "r" #'my/dashboard-refresh
      :n "q" #'my/dashboard-quit)

;; ============================================================================
;; ENTRY POINT
;; ============================================================================

(defun my/dashboard-open ()
  "Open the unified dashboard with split windows.
Left: org-agenda week view, Right: habits and todos."
  (interactive)
  ;; Delete other windows to start fresh
  (delete-other-windows)

  ;; Create the habits/todos buffer first
  (let ((dash-buf (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer dash-buf
      (my/dashboard-mode)
      (my/dashboard-render-right-pane))

    ;; Split window - agenda on left, dashboard on right
    (split-window-right)

    ;; Left window: org-agenda
    (org-agenda nil "a")

    ;; Right window: habits/todos dashboard
    (other-window 1)
    (switch-to-buffer dash-buf)))

(defun my/dashboard-render-right-pane ()
  "Render the right pane (deadlines + habits + todos)."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert (propertize
             (format "═══════════════════════════════════════\n")
             'face 'font-lock-comment-face))
    (insert (propertize
             (format "        %s\n"
                     (format-time-string "%A, %b %d"))
             'face 'bold))
    (insert (propertize
             (format "═══════════════════════════════════════\n\n")
             'face 'font-lock-comment-face))

    ;; Sections
    (my/dashboard-render-agenda-section)
    (my/dashboard-render-habits-section)
    (my/dashboard-render-todos-section)

    ;; Footer
    (insert (propertize
             (format "═══════════════════════════════════════\n")
             'face 'font-lock-comment-face))
    (insert (propertize "Habits: " 'face 'font-lock-comment-face))
    (insert (propertize "RET=toggle c=done i=skip x=cancel\n" 'face 'font-lock-comment-face))
    (insert (propertize "View:   " 'face 'font-lock-comment-face))
    (insert (propertize "r=refresh q=quit SPC w w=switch pane\n" 'face 'font-lock-comment-face))
    (insert (propertize "Todos:  " 'face 'font-lock-comment-face))
    (insert (propertize "SPC o c=capture SPC o t=todo.org\n" 'face 'font-lock-comment-face))

    (goto-char (point-min))))

(provide 'dashboard)
;;; dashboard.el ends here
