;;; habits.el --- Habit tracking system for Doom Emacs -*- lexical-binding: t; -*-

;; A custom habit tracking system with daily and weekly habits,
;; three states (completed, incomplete, cancelled), cancellation date ranges,
;; and automatic logging to JSONL files.

;; ============================================================================
;; CORE VARIABLES
;; ============================================================================

(defvar my/habit-file (expand-file-name "habits.org" org-directory)
  "Path to the habits.org file.")

(defvar my/habit-log-dir (expand-file-name "~/.config/habit-log/")
  "Directory for habit log files.")

(defvar my/habit-daily-log (concat my/habit-log-dir "daily-habits.jsonl")
  "Path to daily habits JSONL log.")

(defvar my/habit-weekly-log (concat my/habit-log-dir "weekly-habits.jsonl")
  "Path to weekly habits JSONL log.")

(defvar my/habit-session-file (concat my/habit-log-dir ".session-state.el")
  "Path to session state file for persistence across Emacs restarts.")

(defvar my/habit-daily-states (make-hash-table :test 'equal)
  "Hash table storing current day's habit states.
Keys are habit names, values are symbols: 'completed, 'incomplete, or 'cancelled.")

(defvar my/habit-weekly-states (make-hash-table :test 'equal)
  "Hash table storing current week's habit states.
Keys are habit names, values are symbols: 'completed, 'incomplete, or 'cancelled.")

(defvar my/habit-current-daily-date nil
  "The date (YYYY-MM-DD string) for which daily states are currently loaded.")

(defvar my/habit-current-week-start nil
  "The Monday date (YYYY-MM-DD string) for which weekly states are currently loaded.")

(defvar my/habit-daily-timer nil
  "Timer for daily habit auto-finalization at midnight.")

(defvar my/habit-weekly-timer nil
  "Timer for weekly habit auto-finalization at midnight Sunday.")

(defvar my/habit-buffer-name "*Habit Review*"
  "Name of the habit review buffer.")

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/habit-ensure-log-dir ()
  "Create habit log directory if it doesn't exist."
  (unless (file-directory-p my/habit-log-dir)
    (make-directory my/habit-log-dir t)))

(defun my/habit-date-to-string (time)
  "Convert Emacs TIME to YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d" time))

(defun my/habit-string-to-date (str)
  "Convert YYYY-MM-DD string to Emacs time."
  (let ((parsed (parse-time-string (concat str " 00:00:00"))))
    (encode-time (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                 (nth 3 parsed) (nth 4 parsed) (nth 5 parsed))))

(defun my/habit-today-string ()
  "Return today's date as YYYY-MM-DD string."
  (my/habit-date-to-string (current-time)))

(defun my/habit-get-week-start (time)
  "Return the Monday of the week containing TIME as YYYY-MM-DD string.
Week starts on Monday."
  (let* ((decoded (decode-time time))
         (dow (nth 6 decoded))  ; 0=Sunday, 1=Monday, ..., 6=Saturday
         ;; Adjust Sunday (0) to be day 7 for calculation
         (adjusted-dow (if (= dow 0) 7 dow))
         ;; Days since Monday
         (days-since-monday (- adjusted-dow 1))
         ;; Subtract days to get to Monday
         (monday-time (time-subtract time (days-to-time days-since-monday))))
    (my/habit-date-to-string monday-time)))

(defun my/habit-get-week-end (week-start-str)
  "Return the Sunday date for a week starting on WEEK-START-STR (Monday)."
  (let ((monday-time (my/habit-string-to-date week-start-str)))
    (my/habit-date-to-string (time-add monday-time (days-to-time 6)))))

(defun my/habit-current-week-start-string ()
  "Return the Monday of the current week as YYYY-MM-DD string."
  (my/habit-get-week-start (current-time)))

(defun my/habit-iso-timestamp ()
  "Return current time as ISO 8601 timestamp string."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

;; ============================================================================
;; HABIT PARSING FUNCTIONS
;; ============================================================================

(defun my/habit-get-all-habits ()
  "Parse habits.org and return list of (name . type) cons cells.
TYPE is the symbol 'daily or 'weekly.
Uses simple text parsing to avoid org-mode overhead."
  (let ((habits '()))
    (when (file-exists-p my/habit-file)
      (with-temp-buffer
        (insert-file-contents my/habit-file)
        (goto-char (point-min))
        ;; Find all level-2 headings with HABIT_TYPE property
        (while (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
          (let ((name (match-string 1)))
            ;; Skip EXAMPLE entries
            (unless (string-prefix-p "EXAMPLE:" name)
              ;; Look for HABIT_TYPE in the next few lines
              (save-excursion
                (let ((limit (save-excursion
                               (or (re-search-forward "^\\*" nil t) (point-max)))))
                  (when (re-search-forward ":HABIT_TYPE:\\s-*\\(daily\\|weekly\\)" limit t)
                    (push (cons name (intern (match-string 1))) habits)))))))))
    (nreverse habits)))

(defun my/habit-get-habits-by-type (type)
  "Return list of habit names for TYPE ('daily or 'weekly)."
  (let ((all-habits (my/habit-get-all-habits)))
    (mapcar #'car
            (seq-filter (lambda (h) (eq (cdr h) type)) all-habits))))

(defun my/habit-get-cancelled-ranges (habit-name)
  "Return list of (start . end) cons cells for HABIT-NAME.
START and END are YYYY-MM-DD strings. Returns nil if no cancellations.
Uses simple text parsing to avoid org-mode overhead."
  (let ((ranges '()))
    (when (file-exists-p my/habit-file)
      (with-temp-buffer
        (insert-file-contents my/habit-file)
        (goto-char (point-min))
        ;; Find the heading for this habit
        (when (re-search-forward (format "^\\*\\* %s$" (regexp-quote habit-name)) nil t)
          (let ((limit (save-excursion
                         (or (re-search-forward "^\\*" nil t) (point-max)))))
            ;; Look for CANCELLED_RANGES property
            (when (re-search-forward ":CANCELLED_RANGES:\\s-*\\(.+\\)$" limit t)
              (let ((range-str (match-string 1)))
                (when (and range-str (not (string-empty-p (string-trim range-str))))
                  (dolist (range (split-string range-str "," t "[ \t]+"))
                    (when (string-match "\\([0-9-]+\\)--\\([0-9-]+\\)" range)
                      (push (cons (match-string 1 range)
                                  (match-string 2 range))
                            ranges))))))))))
    (nreverse ranges)))

(defun my/habit-date-in-range-p (date-str start-str end-str)
  "Return t if DATE-STR falls within START-STR to END-STR inclusive."
  (and (string>= date-str start-str)
       (string<= date-str end-str)))

(defun my/habit-is-cancelled-p (habit-name date-str)
  "Return t if HABIT-NAME is cancelled on DATE-STR."
  (let ((ranges (my/habit-get-cancelled-ranges habit-name)))
    (seq-some (lambda (range)
                (my/habit-date-in-range-p date-str (car range) (cdr range)))
              ranges)))

(defun my/habit-is-cancelled-for-week-p (habit-name week-start-str)
  "Return t if HABIT-NAME is cancelled for any day in the week starting WEEK-START-STR."
  (let ((week-end-str (my/habit-get-week-end week-start-str)))
    (my/habit-is-cancelled-p habit-name week-start-str)))

;; ============================================================================
;; STATE MANAGEMENT
;; ============================================================================

(defun my/habit-init-daily-states ()
  "Initialize daily states hash table for today.
Sets all daily habits to 'incomplete, or 'cancelled if cancelled today."
  (let ((today (my/habit-today-string)))
    (clrhash my/habit-daily-states)
    (setq my/habit-current-daily-date today)
    (dolist (habit (my/habit-get-habits-by-type 'daily))
      (puthash habit
               (if (my/habit-is-cancelled-p habit today)
                   'cancelled
                 'incomplete)
               my/habit-daily-states))))

(defun my/habit-init-weekly-states ()
  "Initialize weekly states hash table for current week.
Sets all weekly habits to 'incomplete, or 'cancelled if cancelled."
  (let ((week-start (my/habit-current-week-start-string)))
    (clrhash my/habit-weekly-states)
    (setq my/habit-current-week-start week-start)
    (dolist (habit (my/habit-get-habits-by-type 'weekly))
      (puthash habit
               (if (my/habit-is-cancelled-for-week-p habit week-start)
                   'cancelled
                 'incomplete)
               my/habit-weekly-states))))

(defun my/habit-ensure-states-current ()
  "Ensure state hash tables are initialized for current day/week.
Reinitializes if date has changed."
  (let ((today (my/habit-today-string))
        (week-start (my/habit-current-week-start-string)))
    ;; Check if we need to reinitialize daily states
    (unless (string= today my/habit-current-daily-date)
      (my/habit-init-daily-states))
    ;; Check if we need to reinitialize weekly states
    (unless (string= week-start my/habit-current-week-start)
      (my/habit-init-weekly-states))))

(defun my/habit-get-state (habit-name habit-type)
  "Get current state of HABIT-NAME for HABIT-TYPE ('daily or 'weekly)."
  (my/habit-ensure-states-current)
  (let ((hash (if (eq habit-type 'daily)
                  my/habit-daily-states
                my/habit-weekly-states)))
    (or (gethash habit-name hash) 'incomplete)))

(defun my/habit-set-state (habit-name habit-type new-state)
  "Set HABIT-NAME to NEW-STATE for HABIT-TYPE.
NEW-STATE should be 'completed, 'incomplete, or 'cancelled."
  (my/habit-ensure-states-current)
  (let ((hash (if (eq habit-type 'daily)
                  my/habit-daily-states
                my/habit-weekly-states)))
    (puthash habit-name new-state hash)))

(defun my/habit-cycle-state (habit-name habit-type)
  "Cycle HABIT-NAME through states: incomplete -> completed -> cancelled -> incomplete."
  (let* ((current (my/habit-get-state habit-name habit-type))
         (next (pcase current
                 ('incomplete 'completed)
                 ('completed 'cancelled)
                 ('cancelled 'incomplete)
                 (_ 'incomplete))))
    (my/habit-set-state habit-name habit-type next)
    next))

;; ============================================================================
;; JSONL LOGGING
;; ============================================================================

(defun my/habit-states-to-json-object (states-hash)
  "Convert STATES-HASH to JSON object string."
  (let ((pairs '()))
    (maphash (lambda (k v)
               (push (format "\"%s\":\"%s\"" k (symbol-name v)) pairs))
             states-hash)
    (concat "{" (string-join (nreverse pairs) ",") "}")))

(defun my/habit-log-entry-exists-p (log-file key-field key-value)
  "Check if an entry with KEY-FIELD equal to KEY-VALUE exists in LOG-FILE."
  (when (file-exists-p log-file)
    (with-temp-buffer
      (insert-file-contents log-file)
      (let ((found nil))
        (goto-char (point-min))
        (while (and (not found) (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (and (not (string-empty-p line))
                       (string-match (format "\"%s\":\"%s\"" key-field key-value) line))
              (setq found t)))
          (forward-line 1))
        found))))

(defun my/habit-write-daily-log (date states-hash finalized-by)
  "Write daily habit log entry for DATE with STATES-HASH.
FINALIZED-BY is either \"timer\" or \"manual\"."
  (my/habit-ensure-log-dir)
  ;; Check for duplicate
  (when (my/habit-log-entry-exists-p my/habit-daily-log "date" date)
    (message "Habit log: Entry for %s already exists, skipping." date)
    (cl-return-from my/habit-write-daily-log nil))
  ;; Build and write entry
  (let ((entry (format "{\"date\":\"%s\",\"logged_at\":\"%s\",\"habits\":%s,\"finalized_by\":\"%s\"}\n"
                       date
                       (my/habit-iso-timestamp)
                       (my/habit-states-to-json-object states-hash)
                       finalized-by)))
    (with-temp-buffer
      (insert entry)
      (append-to-file (point-min) (point-max) my/habit-daily-log))
    (message "Habit log: Recorded daily habits for %s" date)))

(defun my/habit-write-weekly-log (week-start week-end states-hash finalized-by)
  "Write weekly habit log entry for WEEK-START to WEEK-END with STATES-HASH.
FINALIZED-BY is either \"timer\" or \"manual\"."
  (my/habit-ensure-log-dir)
  ;; Check for duplicate
  (when (my/habit-log-entry-exists-p my/habit-weekly-log "week_start" week-start)
    (message "Habit log: Entry for week %s already exists, skipping." week-start)
    (cl-return-from my/habit-write-weekly-log nil))
  ;; Build and write entry
  (let ((entry (format "{\"week_start\":\"%s\",\"week_end\":\"%s\",\"logged_at\":\"%s\",\"habits\":%s,\"finalized_by\":\"%s\"}\n"
                       week-start
                       week-end
                       (my/habit-iso-timestamp)
                       (my/habit-states-to-json-object states-hash)
                       finalized-by)))
    (with-temp-buffer
      (insert entry)
      (append-to-file (point-min) (point-max) my/habit-weekly-log))
    (message "Habit log: Recorded weekly habits for week of %s" week-start)))

;; ============================================================================
;; TIMER FUNCTIONS
;; ============================================================================

(defun my/habit-calculate-next-midnight ()
  "Return Emacs time for next midnight (00:00:00 tomorrow)."
  (let* ((now (decode-time (current-time)))
         (tomorrow (encode-time 0 0 0
                                (1+ (nth 3 now))  ; day + 1
                                (nth 4 now)       ; month
                                (nth 5 now))))    ; year
    tomorrow))

(defun my/habit-calculate-next-sunday-midnight ()
  "Return Emacs time for next Sunday at midnight (00:00:00).
This is when the week ends and weekly habits are finalized."
  (let* ((now (decode-time (current-time)))
         (dow (nth 6 now))  ; 0=Sunday, 1=Monday, ..., 6=Saturday
         ;; Days until Sunday (0 if today is Sunday, but we want NEXT Sunday)
         (days-until-sunday (if (= dow 0) 7 (- 7 dow)))
         (sunday (time-add (current-time) (days-to-time days-until-sunday))))
    ;; Set to midnight
    (let ((sunday-decoded (decode-time sunday)))
      (encode-time 0 0 0
                   (nth 3 sunday-decoded)
                   (nth 4 sunday-decoded)
                   (nth 5 sunday-decoded)))))

(defun my/habit-daily-timer-callback ()
  "Called at midnight to finalize the previous day's habits."
  ;; The current states are for "yesterday" now that it's past midnight
  ;; Log them before reinitializing
  (when my/habit-current-daily-date
    (let ((states-copy (copy-hash-table my/habit-daily-states)))
      (my/habit-write-daily-log my/habit-current-daily-date states-copy "timer")))
  ;; Initialize states for the new day
  (my/habit-init-daily-states)
  ;; Schedule next timer
  (my/habit-schedule-daily-timer))

(defun my/habit-weekly-timer-callback ()
  "Called at midnight Sunday to finalize the week's habits."
  ;; The current states are for "last week" now that it's Sunday midnight
  (when my/habit-current-week-start
    (let ((states-copy (copy-hash-table my/habit-weekly-states))
          (week-end (my/habit-get-week-end my/habit-current-week-start)))
      (my/habit-write-weekly-log my/habit-current-week-start week-end states-copy "timer")))
  ;; Initialize states for the new week
  (my/habit-init-weekly-states)
  ;; Schedule next timer
  (my/habit-schedule-weekly-timer))

(defun my/habit-schedule-daily-timer ()
  "Schedule the daily habit finalization timer for next midnight."
  (when my/habit-daily-timer
    (cancel-timer my/habit-daily-timer))
  (let ((next-midnight (my/habit-calculate-next-midnight)))
    (setq my/habit-daily-timer
          (run-at-time next-midnight nil #'my/habit-daily-timer-callback))
    (message "Habit timer: Daily finalization scheduled for %s"
             (my/habit-date-to-string next-midnight))))

(defun my/habit-schedule-weekly-timer ()
  "Schedule the weekly habit finalization timer for next Sunday midnight."
  (when my/habit-weekly-timer
    (cancel-timer my/habit-weekly-timer))
  (let ((next-sunday (my/habit-calculate-next-sunday-midnight)))
    (setq my/habit-weekly-timer
          (run-at-time next-sunday nil #'my/habit-weekly-timer-callback))
    (message "Habit timer: Weekly finalization scheduled for %s"
             (my/habit-date-to-string next-sunday))))

(defun my/habit-start-timers ()
  "Start both daily and weekly habit timers."
  (interactive)
  (my/habit-schedule-daily-timer)
  (my/habit-schedule-weekly-timer)
  (message "Habit timers started."))

(defun my/habit-stop-timers ()
  "Stop all habit timers."
  (interactive)
  (when my/habit-daily-timer
    (cancel-timer my/habit-daily-timer)
    (setq my/habit-daily-timer nil))
  (when my/habit-weekly-timer
    (cancel-timer my/habit-weekly-timer)
    (setq my/habit-weekly-timer nil))
  (message "Habit timers stopped."))

;; ============================================================================
;; MANUAL FINALIZATION
;; ============================================================================

(defun my/habit-finalize-day ()
  "Manually finalize today's habits before midnight."
  (interactive)
  (my/habit-ensure-states-current)
  (let ((today my/habit-current-daily-date))
    (if (my/habit-log-entry-exists-p my/habit-daily-log "date" today)
        (message "Today's habits have already been finalized.")
      (when (yes-or-no-p (format "Finalize daily habits for %s? " today))
        (my/habit-write-daily-log today my/habit-daily-states "manual")
        (message "Daily habits finalized for %s" today)))))

(defun my/habit-finalize-week ()
  "Manually finalize this week's habits before Sunday midnight."
  (interactive)
  (my/habit-ensure-states-current)
  (let* ((week-start my/habit-current-week-start)
         (week-end (my/habit-get-week-end week-start)))
    (if (my/habit-log-entry-exists-p my/habit-weekly-log "week_start" week-start)
        (message "This week's habits have already been finalized.")
      (when (yes-or-no-p (format "Finalize weekly habits for week of %s? " week-start))
        (my/habit-write-weekly-log week-start week-end my/habit-weekly-states "manual")
        (message "Weekly habits finalized for week of %s" week-start)))))

;; ============================================================================
;; CANCELLATION MANAGEMENT
;; ============================================================================

(defun my/habit-add-cancelled-range (habit-name start-date end-date)
  "Add cancellation range for HABIT-NAME from START-DATE to END-DATE.
Modifies the CANCELLED_RANGES property in habits.org."
  (with-current-buffer (find-file-noselect my/habit-file)
    (org-map-entries
     (lambda ()
       (when (string= (org-get-heading t t t t) habit-name)
         (let* ((current (or (org-entry-get nil "CANCELLED_RANGES") ""))
                (new-range (format "%s--%s" start-date end-date))
                (new-value (if (string-empty-p current)
                               new-range
                             (concat current ", " new-range))))
           (org-entry-put nil "CANCELLED_RANGES" new-value))))
     nil 'file)
    (save-buffer)))

(defun my/habit-remove-cancelled-range (habit-name start-date end-date)
  "Remove cancellation range for HABIT-NAME from START-DATE to END-DATE."
  (with-current-buffer (find-file-noselect my/habit-file)
    (org-map-entries
     (lambda ()
       (when (string= (org-get-heading t t t t) habit-name)
         (let* ((current (or (org-entry-get nil "CANCELLED_RANGES") ""))
                (range-to-remove (format "%s--%s" start-date end-date))
                (ranges (split-string current "," t "[ \t]+"))
                (filtered (seq-remove (lambda (r)
                                        (string= (string-trim r) range-to-remove))
                                      ranges))
                (new-value (string-join filtered ", ")))
           (org-entry-put nil "CANCELLED_RANGES" new-value))))
     nil 'file)
    (save-buffer)))

(defun my/habit-cancel-range (habit-name start-date end-date)
  "Cancel HABIT-NAME from START-DATE to END-DATE.
Prompts interactively if called without arguments."
  (interactive
   (let* ((habits (mapcar #'car (my/habit-get-all-habits)))
          (habit (completing-read "Cancel habit: " habits nil t))
          (start (org-read-date nil nil nil "Start date: "))
          (end (org-read-date nil nil nil "End date: ")))
     (list habit start end)))
  (my/habit-add-cancelled-range habit-name start-date end-date)
  (message "Cancelled %s from %s to %s" habit-name start-date end-date)
  ;; Reinitialize states if cancellation affects current period
  (my/habit-init-daily-states)
  (my/habit-init-weekly-states)
  ;; Refresh habit buffer if open
  (when (get-buffer my/habit-buffer-name)
    (my/habit-refresh-agenda)))

(defun my/habit-uncancel-range ()
  "Remove a cancellation range from a habit.
Shows only habits with existing cancellations."
  (interactive)
  (let* ((all-habits (my/habit-get-all-habits))
         (habits-with-cancellations
          (seq-filter (lambda (h)
                        (my/habit-get-cancelled-ranges (car h)))
                      all-habits)))
    (if (null habits-with-cancellations)
        (message "No habits have cancellation ranges.")
      (let* ((habit-name (completing-read "Uncancel habit: "
                                          (mapcar #'car habits-with-cancellations)
                                          nil t))
             (ranges (my/habit-get-cancelled-ranges habit-name))
             (range-strings (mapcar (lambda (r) (format "%s to %s" (car r) (cdr r)))
                                    ranges))
             (selected (completing-read "Remove cancellation: " range-strings nil t))
             (idx (seq-position range-strings selected))
             (range (nth idx ranges)))
        (my/habit-remove-cancelled-range habit-name (car range) (cdr range))
        (message "Removed cancellation for %s: %s to %s" habit-name (car range) (cdr range))
        ;; Reinitialize states
        (my/habit-init-daily-states)
        (my/habit-init-weekly-states)
        ;; Refresh habit buffer if open
        (when (get-buffer my/habit-buffer-name)
          (my/habit-refresh-agenda))))))

(defun my/habit-view-cancellations ()
  "Display all habit cancellation ranges in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Habit Cancellations*"))
        (all-habits (my/habit-get-all-habits)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "=== Habit Cancellation Ranges ===\n\n")
      (let ((any-cancellations nil))
        (dolist (habit all-habits)
          (let ((ranges (my/habit-get-cancelled-ranges (car habit))))
            (when ranges
              (setq any-cancellations t)
              (insert (format "%s (%s):\n" (car habit) (cdr habit)))
              (dolist (range ranges)
                (insert (format "  - %s to %s\n" (car range) (cdr range))))
              (insert "\n"))))
        (unless any-cancellations
          (insert "No cancellation ranges configured.\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))

;; ============================================================================
;; HABIT REVIEW AGENDA VIEW
;; ============================================================================

(defvar my/habit-mode-map
  (make-sparse-keymap)
  "Keymap for habit review buffer.")

(define-derived-mode my/habit-mode special-mode "Habits"
  "Major mode for reviewing and toggling habits."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Set up evil keybindings when the mode is activated
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'my/habit-mode 'normal)))

(defun my/habit-quit ()
  "Quit habit view and return to dashboard."
  (interactive)
  (kill-current-buffer)
  (+doom-dashboard/open (selected-frame)))

;; Evil keybindings for habit mode using Doom's map!
(map! :map my/habit-mode-map
      :n "RET" #'my/habit-cycle-at-point
      :n "c" #'my/habit-mark-completed-at-point
      :n "i" #'my/habit-mark-incomplete-at-point
      :n "x" #'my/habit-mark-cancelled-at-point
      :n "r" #'my/habit-refresh-agenda
      :n "q" #'my/habit-quit)

(defun my/habit-get-status-indicator (state)
  "Return visual indicator for STATE."
  (pcase state
    ('completed "[X]")
    ('incomplete "[ ]")
    ('cancelled "[-]")
    (_ "[?]")))

(defun my/habit-get-status-face (state)
  "Return face for STATE."
  (pcase state
    ('completed 'success)
    ('incomplete 'default)
    ('cancelled 'shadow)
    (_ 'default)))

(defun my/habit-format-entry (habit-name habit-type state description)
  "Format a habit entry for display.
Returns a propertized string."
  (let* ((indicator (my/habit-get-status-indicator state))
         (face (my/habit-get-status-face state))
         (cancelled-note (if (eq state 'cancelled) " (cancelled)" ""))
         (line (format "%s %-25s %s%s"
                       indicator
                       habit-name
                       (or description "")
                       cancelled-note)))
    (propertize line
                'face face
                'habit-name habit-name
                'habit-type habit-type
                'habit-state state)))

(defun my/habit-get-description (habit-name)
  "Get the description text for HABIT-NAME from habits.org.
Uses simple text parsing to avoid org-mode overhead."
  (when (file-exists-p my/habit-file)
    (with-temp-buffer
      (insert-file-contents my/habit-file)
      (goto-char (point-min))
      ;; Find the heading for this habit
      (when (re-search-forward (format "^\\*\\* %s$" (regexp-quote habit-name)) nil t)
        (let ((limit (save-excursion
                       (or (re-search-forward "^\\*" nil t) (point-max)))))
          ;; Skip past the :PROPERTIES: drawer
          (when (re-search-forward "^:END:" limit t)
            (forward-line 1)
            (let ((desc-start (point)))
              ;; Get text until next heading or end
              (goto-char limit)
              (when (> limit desc-start)
                (let ((desc (string-trim (buffer-substring-no-properties desc-start limit))))
                  (unless (string-empty-p desc)
                    desc))))))))))

(defun my/habit-render-agenda ()
  "Render the habit review buffer content."
  (my/habit-ensure-states-current)
  (let ((inhibit-read-only t)
        (daily-habits (my/habit-get-habits-by-type 'daily))
        (weekly-habits (my/habit-get-habits-by-type 'weekly))
        (today (my/habit-today-string))
        (week-start my/habit-current-week-start)
        (week-end (my/habit-get-week-end my/habit-current-week-start)))
    (erase-buffer)
    ;; Header
    (insert (propertize
             (format "================================================================================\n")
             'face 'font-lock-comment-face))
    (insert (propertize
             (format "                     Habit Review - %s\n"
                     (format-time-string "%A, %b %d, %Y"))
             'face 'bold))
    (insert (propertize
             (format "================================================================================\n\n")
             'face 'font-lock-comment-face))

    ;; Daily Habits Section
    (insert (propertize "Daily Habits" 'face 'bold))
    (insert (format " (finalize by midnight tonight)\n"))
    (insert (make-string 50 ?-) "\n")
    (if daily-habits
        (dolist (habit daily-habits)
          (let ((state (my/habit-get-state habit 'daily))
                (desc (my/habit-get-description habit)))
            (insert (my/habit-format-entry habit 'daily state desc) "\n")))
      (insert "(No daily habits defined)\n"))
    (insert "\n")

    ;; Weekly Habits Section
    (insert (propertize "Weekly Habits" 'face 'bold))
    (insert (format " (finalize by midnight Sunday)\n"))
    (insert (format "Week of %s - %s\n" week-start week-end))
    (insert (make-string 50 ?-) "\n")
    (if weekly-habits
        (dolist (habit weekly-habits)
          (let ((state (my/habit-get-state habit 'weekly))
                (desc (my/habit-get-description habit)))
            (insert (my/habit-format-entry habit 'weekly state desc) "\n")))
      (insert "(No weekly habits defined)\n"))
    (insert "\n")

    ;; Footer with keybindings
    (insert (propertize
             (format "--------------------------------------------------------------------------------\n")
             'face 'font-lock-comment-face))
    (insert (propertize
             "Keys: RET=cycle  c=complete  i=incomplete  x=cancel  r=refresh  q=quit\n"
             'face 'font-lock-comment-face))
    (insert (propertize
             (format "================================================================================\n")
             'face 'font-lock-comment-face))

    (goto-char (point-min))
    ;; Move to first habit line
    (re-search-forward "^\\[" nil t)
    (beginning-of-line)))

(defun my/habit-agenda-view ()
  "Display the habit review agenda view."
  (interactive)
  (let ((buf (get-buffer-create my/habit-buffer-name)))
    (with-current-buffer buf
      (my/habit-mode)
      (my/habit-render-agenda))
    (switch-to-buffer buf)))

(defun my/habit-refresh-agenda ()
  "Refresh the habit review buffer."
  (interactive)
  (when (get-buffer my/habit-buffer-name)
    (with-current-buffer my/habit-buffer-name
      (let ((pos (point)))
        (my/habit-render-agenda)
        (goto-char (min pos (point-max)))))))

(defun my/habit-get-habit-at-point ()
  "Return (name . type) of habit at point, or nil."
  (let ((name (get-text-property (point) 'habit-name))
        (type (get-text-property (point) 'habit-type)))
    (when (and name type)
      (cons name type))))

(defun my/habit-cycle-at-point ()
  "Cycle the habit state at point."
  (interactive)
  (let ((habit (my/habit-get-habit-at-point)))
    (if habit
        (let ((new-state (my/habit-cycle-state (car habit) (cdr habit))))
          (my/habit-refresh-agenda)
          (message "%s: %s" (car habit) new-state))
      (message "No habit at point"))))

(defun my/habit-mark-completed-at-point ()
  "Mark habit at point as completed."
  (interactive)
  (let ((habit (my/habit-get-habit-at-point)))
    (if habit
        (progn
          (my/habit-set-state (car habit) (cdr habit) 'completed)
          (my/habit-refresh-agenda)
          (message "%s: completed" (car habit)))
      (message "No habit at point"))))

(defun my/habit-mark-incomplete-at-point ()
  "Mark habit at point as incomplete."
  (interactive)
  (let ((habit (my/habit-get-habit-at-point)))
    (if habit
        (progn
          (my/habit-set-state (car habit) (cdr habit) 'incomplete)
          (my/habit-refresh-agenda)
          (message "%s: incomplete" (car habit)))
      (message "No habit at point"))))

(defun my/habit-mark-cancelled-at-point ()
  "Mark habit at point as cancelled."
  (interactive)
  (let ((habit (my/habit-get-habit-at-point)))
    (if habit
        (progn
          (my/habit-set-state (car habit) (cdr habit) 'cancelled)
          (my/habit-refresh-agenda)
          (message "%s: cancelled" (car habit)))
      (message "No habit at point"))))

;; ============================================================================
;; LOG VIEWING
;; ============================================================================

(defun my/habit-view-logs ()
  "Display recent habit log entries."
  (interactive)
  (let ((buf (get-buffer-create "*Habit Logs*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "=== Recent Habit Logs ===\n\n")

      ;; Daily logs
      (insert "--- Daily Habits ---\n")
      (if (file-exists-p my/habit-daily-log)
          (let ((lines (with-temp-buffer
                         (insert-file-contents my/habit-daily-log)
                         (split-string (buffer-string) "\n" t))))
            (dolist (line (last lines 10))
              (insert line "\n")))
        (insert "(No daily logs yet)\n"))
      (insert "\n")

      ;; Weekly logs
      (insert "--- Weekly Habits ---\n")
      (if (file-exists-p my/habit-weekly-log)
          (let ((lines (with-temp-buffer
                         (insert-file-contents my/habit-weekly-log)
                         (split-string (buffer-string) "\n" t))))
            (dolist (line (last lines 10))
              (insert line "\n")))
        (insert "(No weekly logs yet)\n"))

      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))

;; ============================================================================
;; SESSION PERSISTENCE
;; ============================================================================

(defun my/habit-save-session-states ()
  "Save current session states to a file.
Called before Emacs exits."
  (my/habit-ensure-log-dir)
  (with-temp-file my/habit-session-file
    (insert ";; Habit session state - auto-generated\n")
    (insert (format "(setq my/habit-current-daily-date %S)\n" my/habit-current-daily-date))
    (insert (format "(setq my/habit-current-week-start %S)\n" my/habit-current-week-start))
    ;; Save daily states
    (insert "(clrhash my/habit-daily-states)\n")
    (maphash (lambda (k v)
               (insert (format "(puthash %S '%s my/habit-daily-states)\n" k v)))
             my/habit-daily-states)
    ;; Save weekly states
    (insert "(clrhash my/habit-weekly-states)\n")
    (maphash (lambda (k v)
               (insert (format "(puthash %S '%s my/habit-weekly-states)\n" k v)))
             my/habit-weekly-states)))

(defun my/habit-load-session-states ()
  "Load session states from file if valid.
Only loads if the saved states are for the current day/week."
  (when (file-exists-p my/habit-session-file)
    (let ((today (my/habit-today-string))
          (week-start (my/habit-current-week-start-string)))
      ;; Load the file to get saved dates
      (load my/habit-session-file t t)
      ;; Check if saved states are current
      (cond
       ;; Both current - keep loaded states
       ((and (string= my/habit-current-daily-date today)
             (string= my/habit-current-week-start week-start))
        (message "Habit states restored from previous session."))
       ;; Daily is stale, weekly is current
       ((and (not (string= my/habit-current-daily-date today))
             (string= my/habit-current-week-start week-start))
        (my/habit-init-daily-states)
        (message "Daily habit states reinitialized (new day). Weekly states restored."))
       ;; Daily is current, weekly is stale
       ((and (string= my/habit-current-daily-date today)
             (not (string= my/habit-current-week-start week-start)))
        (my/habit-init-weekly-states)
        (message "Daily states restored. Weekly habit states reinitialized (new week)."))
       ;; Both stale
       (t
        (my/habit-init-daily-states)
        (my/habit-init-weekly-states)
        (message "Habit states reinitialized (new day and week).")))
      ;; Delete session file after loading
      (delete-file my/habit-session-file))))

;; ============================================================================
;; OPEN HABITS FILE
;; ============================================================================

(defun my/habit-open-file ()
  "Open the habits.org file for editing."
  (interactive)
  (find-file my/habit-file))

;; ============================================================================
;; INITIALIZATION
;; ============================================================================

(defun my/habit-initialize ()
  "Initialize the habit tracking system.
Called during Emacs startup."
  (my/habit-ensure-log-dir)
  ;; Try to load session states first
  (if (file-exists-p my/habit-session-file)
      (my/habit-load-session-states)
    ;; Otherwise initialize fresh
    (my/habit-init-daily-states)
    (my/habit-init-weekly-states))
  ;; Start timers
  (my/habit-start-timers)
  ;; Save states on exit
  (add-hook 'kill-emacs-hook #'my/habit-save-session-states))

;; Auto-initialize when this file is loaded
(my/habit-initialize)

(provide 'habits)
;;; habits.el ends here
