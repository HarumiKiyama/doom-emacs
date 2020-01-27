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

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:

(setq display-line-numbers-type `relative
      scroll-margin 5)

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

(defun private/create-algorithm-org-file ()
  (interactive)
  (let ((name (read-string "leetcode title:")))
    (expand-file-name (format "%s.org" name)
                      "~/projects/AlgorithmPractice/Leetcode/")))

;; org configs
(after! org
  (setq org-babel-eval-verbose t
        org-directory "~/org-mode/"
        org-id-locations-file (concat org-directory ".orgids")
        org-version "9.3.0"
        org-tag-alist '(("Routine" . ?r)
                        ("Algorithms" . ?a)
                        ("Reading" . ?R))
        org-capture-templates '(("w" "Words" entry (file+headline "Esperanto.org" "Words")
                                 "** word :drill:\n%^{Esperanto}[%^{English}]")
                                ("e" "Emacs" entry (file+headline "task.org" "Emacs Hacking") "*** TODO %?")
                                ("a" "Algorithm" entry (file private/create-algorithm-org-file) "* Description\n%?\n* Solution"))
        org-archive-location "~/org-mode/archive.org::"
        org-startup-truncated nil
        ;; org-journal setting
        org-journal-date-format "%Y-%m-%d %A"
        org-journal-time-format ""
        org-journal-time-prefix ""
        org-startup-folded 'showall
        org-log-into-drawer t)
  (add-to-list 'org-src-lang-modes '("rust" . rustic)))

(after! magit
  (setq magit-version "2.90.1.1"))

;; keyfreq config
(use-package! keyfreq
  :init
  (set-popup-rules!
        '(("^\\*frequencies" :size 8 :select t)))
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          evil-force-normal-state
          abort-recursive-edit
          ivy-next-line
          ivy-previous-line
          evil-next-line
          evil-previous-line
          forward-char
          backward-char
          previous-line
          next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)

  )

(defun org-push-private ()
  "git commit and push to private remote repo"
  (interactive)
  (call-process-shell-command "cd ~/org-mode && git add .&&git commit -m \"$(date +%Y/%m/%d)\"&&git push" nil 0 0))
