;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Harumi Kiyama"
      user-mail-address "h.kiyama0720@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Dejavu Sans Mono" :size 24))
(setq confirm-kill-emacs nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'zenburn)
(setq +lookup-open-url-fn #'eww)


;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org-mode")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:

(setq display-line-numbers-type `relative)

(setq +evil-want-o/O-to-continue-comments nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; org configs
(with-eval-after-load 'org
  (setq org-babel-eval-verbose t
        org-directory "~/org-mode"
        org-agenda-span 'day
        org-agenda-start-day nil
        org-todo-keywords '((sequence "TODO(t)" "TESTING(t)" "SUSPEND(p)" "|"
                                      "DONE(d!)" "ABORT(a)"))
        org-tag-alist '(("routine" . ?r)
                        ("Algorithms" . ?a)
                        ("Reading" . ?R))
        org-capture-templates '(("w" "Words" entry (file+headline "~/org-mode/Esperanto.org" "Words")
                                 "** word :drill:\n%^{Esperanto}[%^{English}]")
                                )
        org-agenda-files '("~/org-mode/task.org"
                           "~/org-mode/notation.org"
                           "~/org-mode/routine.org"
                           "~/org-mode/blog.org")
        org-refile-targets '(("~/org-mode/task.org" :maxlevel . 1)
                             ("~/org-mode/notes.org" :maxlevel . 1)
                             ("~/org-mode/someday.org" :maxlevel . 1)
                             ("~/org-mode/blog.org" :maxlevel . 1)
                             (nil . (:maxlevel . 2)))
        org-archive-location "~/org-mode/archive.org::"
        org-startup-truncated nil)
  ;; org-journal setting
  (setq org-journal-date-format "%Y-%m-%d %A"
        org-journal-time-format ""
        org-journal-time-prefix "")
  (setq org-startup-folded 'showall)
  (add-to-list 'org-src-lang-modes '("rust" . rustic))
  )

;; (with-timeout  (start-process-shell-command "git-pull" nil "cd ~/org-mode&& git pull"))
;; (add-hook 'kill-emacs-hook (lambda ()
                              ;; (call-process-shell-command "cd ~/org-mode && git add .&&git commit -m \"$(date +%Y/%m/%d)\"&&git push" nil 0 0)))
