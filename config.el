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

;; Capture templates for quick note/todo entry
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
           "* TODO %?\n%U\n")
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %?\n%U\n")
          ("r" "Research Note" entry (file+headline "research/papers.org" "Reading Notes")
           "* %?\n%U\n%a\n")
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\n%U\n"))))

;; Org-roam configuration
(setq org-roam-directory (concat org-directory "roam/"))

;; ============================================================================
;; LATEX CONFIGURATION
;; ============================================================================

;; Use pdf-tools for viewing
(setq +latex-viewers '(pdf-tools))

;; Auto-revert PDF on recompile
(after! latex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Default to latexmk
(setq TeX-command-default "LatexMk")

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
