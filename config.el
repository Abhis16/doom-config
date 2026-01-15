;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Set default frame (window) size - wider than tall
(add-to-list 'default-frame-alist '(width . 170))   ; characters wide
(add-to-list 'default-frame-alist '(height . 70))   ; lines tall

;; Ensure proper font fallbacks to prevent rendering crashes
(setq doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))

;; ;; Disable modeline icons to prevent font rendering crashes
;; (setq doom-modeline-icon nil)

;; ;; Set up Unicode font fallbacks to prevent crashes on special characters
;; (defun my/setup-font-fallbacks ()
;;   "Set up font fallbacks for Unicode characters."
;;   (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
;;   (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)
;;   (set-fontset-font t 'unicode "Apple Symbols" nil 'append)
;;   (set-fontset-font t 'unicode "Symbol" nil 'append))

;; (add-hook 'after-init-hook #'my/setup-font-fallbacks)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; ============================================================================
;; ORG-MODE CONFIGURATION
;; ============================================================================

;; Set org directory to iCloud for Beorg sync
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

;; Agenda files
(setq org-agenda-files (list org-directory))

;; Todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; ============================================================================
;; TODO SYSTEM: TAG CONFIGURATION
;; ============================================================================

;; Define three mutually exclusive tags for task categorization
(after! org
  (setq org-tag-alist
        '((:startgroup)
          ("personal" . ?p)
          ("research" . ?r)
          ("school" . ?s)
          (:endgroup)))

  ;; Prevent tag inheritance (each task must have its own tag)
  (setq org-tags-exclude-from-inheritance '("personal" "research" "school"))

  ;; Display tags prominently in agenda
  (setq org-agenda-tags-column -100))

;; ============================================================================
;; TODO SYSTEM: DEADLINE ENFORCEMENT
;; ============================================================================

(defun my/org-capture-ensure-deadline ()
  "Ensure the captured task has a deadline. Prompt if missing."
  (save-excursion
    (org-back-to-heading t)
    (unless (org-entry-get nil "DEADLINE")
      (org-deadline nil))))

(defun my/org-capture-todo-finalize ()
  "Hook to ensure todos have deadlines."
  (when (member (org-capture-get :key) '("tp" "tr" "ts"))
    (my/org-capture-ensure-deadline)))

(add-hook 'org-capture-before-finalize-hook #'my/org-capture-todo-finalize)

;; Capture templates for quick note/todo entry
(after! org
  (setq org-capture-templates
        '(("t" "Todo")
          ("tp" "Personal Todo" entry (file+headline "todo.org" "Active Tasks")
           "* TODO %? :personal:\n")
          ("tr" "Research Todo" entry (file+headline "todo.org" "Active Tasks")
           "* TODO %? :research:\n")
          ("ts" "School/Admin Todo" entry (file+headline "todo.org" "Active Tasks")
           "* TODO %? :school:\n")
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %?\n%U\n")
          ("r" "Research Note" entry (file+headline "research/papers.org" "Reading Notes")
           "* %?\n%U\n%a\n")
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\n%U\n"))))

;; Quick org navigation and capture shortcuts.
;; Note: SPC o a is handled by Doom's defaults (opens agenda dispatcher)
(map! :leader
      (:prefix ("o" . "org")
       :desc "Capture" "c" #'org-capture
       :desc "Todo file" "t"
       (cmd! (find-file (expand-file-name "todo.org" org-directory)))
       :desc "Notes file" "n"
       (cmd! (find-file (expand-file-name "notes.org" org-directory)))
       :desc "Journal file" "j"
       (cmd! (find-file (expand-file-name "journal.org" org-directory)))
       :desc "Papers file" "p"
       (cmd! (find-file (expand-file-name "research/papers.org" org-directory)))
       ;; Quick access to custom agenda views
       :desc "Top 5 per category" "5" (cmd! (org-agenda nil "t"))))

;; Ensure org local-leader keys for scheduling and todo toggles are consistent.
(map! :map org-mode-map
      :localleader
      :desc "Schedule" "s" #'org-schedule
      :desc "Deadline" "d" #'org-deadline
      :desc "Todo state" "t" #'org-todo)

;; ============================================================================
;; TODO SYSTEM: CUSTOM AGENDA VIEWS
;; ============================================================================

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("t" "Top 5 Per Category"
           ((tags-todo "personal"
                       ((org-agenda-overriding-header "Personal (Top 5)")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notdeadline))
                        (org-agenda-sorting-strategy '(deadline-up priority-down))
                        (org-agenda-max-entries 5)))
            (tags-todo "research"
                       ((org-agenda-overriding-header "\nResearch (Top 5)")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notdeadline))
                        (org-agenda-sorting-strategy '(deadline-up priority-down))
                        (org-agenda-max-entries 5)))
            (tags-todo "school"
                       ((org-agenda-overriding-header "\nSchool/Admin (Top 5)")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notdeadline))
                        (org-agenda-sorting-strategy '(deadline-up priority-down))
                        (org-agenda-max-entries 5))))
           ((org-agenda-files (list (expand-file-name "todo.org" org-directory)))))

          ("p" "All Personal Tasks"
           tags-todo "personal"
           ((org-agenda-overriding-header "All Personal Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(deadline-up priority-down))
            (org-agenda-prefix-format "  %-12:c %?-12t %s")
            (org-agenda-files (list (expand-file-name "todo.org" org-directory)))))

          ("r" "All Research Tasks"
           tags-todo "research"
           ((org-agenda-overriding-header "All Research Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(deadline-up priority-down))
            (org-agenda-prefix-format "  %-12:c %?-12t %s")
            (org-agenda-files (list (expand-file-name "todo.org" org-directory)))))

          ("s" "All School/Admin Tasks"
           tags-todo "school"
           ((org-agenda-overriding-header "All School/Admin Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(deadline-up priority-down))
            (org-agenda-prefix-format "  %-12:c %?-12t %s")
            (org-agenda-files (list (expand-file-name "todo.org" org-directory)))))))

  ;; Additional agenda settings for better display
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; Show deadlines prominently
  (setq org-agenda-deadline-faces
        '((1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))

  ;; Cleaner deadline display
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "Due in %2d: " "Overdue %2d: ")))

;; ============================================================================
;; TODO SYSTEM: ARCHIVE FUNCTIONALITY
;; ============================================================================

;; Move completed tasks to "Completed" section
(after! org
  (setq org-archive-location (concat (expand-file-name "todo.org" org-directory) "::* Completed"))

  ;; Function to archive all DONE tasks
  (defun my/org-archive-done-tasks ()
    "Archive all DONE tasks in the current buffer."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))

  ;; Add keybinding to manually trigger archive
  (map! :map org-mode-map
        :localleader
        :desc "Archive DONE tasks" "A" #'my/org-archive-done-tasks))

;; Org-roam configuration
(setq org-roam-directory (concat org-directory "roam/"))

;; ============================================================================
;; ORG-MODERN: PRETTIER ORG-MODE UI
;; ============================================================================

(use-package! org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-mode . org-modern-agenda))
  :config
  ;; Customize org-modern appearance
  (setq org-modern-star '("◉" "○" "◈" "◇" "✦")
        org-modern-list '((?- . "•") (?+ . "◦") (?* . "‣"))
        org-modern-tag t
        org-modern-priority t
        org-modern-todo t
        org-modern-table t))

;; ============================================================================
;; LATEX CONFIGURATION
;; ============================================================================

;; Use pdf-tools for viewing
(setq +latex-viewers '(pdf-tools))

;; Set master file to the current file by default (avoids nil errors)
(setq-default TeX-master t)

;; Default to latexmk
(setq TeX-command-default "LatexMk")

;; Auto-revert PDF on recompile
(after! latex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; ============================================================================
;; CITAR (ZOTERO) CONFIGURATION
;; ============================================================================

(use-package! citar
  :custom
  (citar-bibliography '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/references.bib"))
  (citar-library-paths '("~/Zotero/storage/"))
  (citar-notes-paths '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/research/")))

;; Use citar with org-cite
(setq org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;; ============================================================================
;; CUSTOM DASHBOARD BANNER
;; ============================================================================

;; Use ASCII banner instead of image
(setq fancy-splash-image nil)

;; Add padding below the banner (top . bottom)
(setq +doom-dashboard-banner-padding '(0 . 8))

;; Custom banner color - cyan/blue gradient look
(custom-set-faces!
  '(doom-dashboard-banner :foreground "#51afef" :weight bold))

(defun my/doom-dashboard-ascii-banner ()
  (let* ((banner '(" /$$      /$$           /$$"
                   "| $$  /$ | $$          | $$"
                   "| $$ /$$$| $$  /$$$$$$ | $$  /$$$$$$$  /$$$$$$  /$$$$$$/$$$$   /$$$$$$"
                   "| $$/$$ $$ $$ /$$__  $$| $$ /$$_____/ /$$__  $$| $$_  $$_  $$ /$$__  $$"
                   "| $$$$_  $$$$| $$$$$$$$| $$| $$      | $$  \\ $$| $$ \\ $$ \\ $$| $$$$$$$$"
                   "| $$$/ \\  $$$| $$_____/| $$| $$      | $$  | $$| $$ | $$ | $$| $$_____/"
                   "| $$/   \\  $$|  $$$$$$$| $$|  $$$$$$$|  $$$$$$/| $$ | $$ | $$|  $$$$$$$ /$$"
                   "|__/     \\__/ \\_______/|__/ \\_______/ \\______/ |__/ |__/ |__/ \\_______/| $/"
                   "                                                                       |_/"
                   ""
                   "  /$$$$$$  /$$       /$$       /$$"
                   " /$$__  $$| $$      | $$      |__/"
                   "| $$  \\ $$| $$$$$$$ | $$$$$$$  /$$"
                   "| $$$$$$$$| $$__  $$| $$__  $$| $$"
                   "| $$__  $$| $$  \\ $$| $$  \\ $$| $$"
                   "| $$  | $$| $$  | $$| $$  | $$| $$"
                   "| $$  | $$| $$$$$$$/| $$  | $$| $$"
                   "|__/  |__/|_______/ |__/  |__/|__/"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)
    ;; Add padding below the banner
    (insert (make-string (or (cdr +doom-dashboard-banner-padding) 4) ?\n))))

(setq +doom-dashboard-ascii-banner-fn #'my/doom-dashboard-ascii-banner)


;; ============================================================================
;; GOOGLE CALENDAR SYNC (Optional - configure when ready)
;; ============================================================================
;; To enable Google Calendar sync:
;; 1. Create a project at https://console.cloud.google.com/
;; 2. Enable Google Calendar API
;; 3. Create OAuth 2.0 credentials
;; 4. Uncomment and fill in the configuration below

;; (use-package! org-gcal
;;   :config
;;   (setq org-gcal-client-id "YOUR_CLIENT_ID"
;;         org-gcal-client-secret "YOUR_CLIENT_SECRET"
;;         org-gcal-fetch-file-alist
;;         '(("your.email@gmail.com" . "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/calendar.org"))))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ============================================================================
;; PERFORMANCE / STABILITY CONFIGURATION
;; ============================================================================

;; LSP performance tuning to reduce memory pressure and prevent crashes
(after! lsp-mode
  (setq lsp-idle-delay 0.5                       ; Delay before LSP refresh
        lsp-log-io nil                           ; Disable IO logging (saves memory)
        gc-cons-threshold (* 100 1024 1024)      ; 100MB GC threshold
        read-process-output-max (* 1024 1024)))  ; 1MB process read buffer

;; Easier exit from insert mode without interfering with typing.
(map! :i "C-[" #'evil-normal-state)

;; If crashes persist, uncomment these lines to disable problematic features:
;; (setq doom-modeline-icon nil)  ; Disable modeline icons
;; (after! pdf-tools (pdf-tools-install :no-query))  ; Force reinstall pdf-tools
