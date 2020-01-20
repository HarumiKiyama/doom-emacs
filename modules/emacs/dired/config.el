;;; tools/dired/config.el -*- lexical-binding: t; -*-

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-auto-revert-buffer t  ; don't prompt to revert; just do it
        dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)

  (let ((args (list "-aBhl" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " ")))

  (add-hook! 'dired-mode-hook
    (defun +dired-disable-gnu-ls-flags-in-tramp-buffers-h ()
      "Fix #1703: dired over TRAMP displays a blank screen.

This is because there's no guarantee the remote system has GNU ls, which is the
only variant that supports --group-directories-first."
      (when (file-remote-p default-directory)
        (setq-local dired-listing-switches
                    (string-join
                     (split-string dired-listing-switches
                                   "--group-directories-first")
                     " ")))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))


(use-package! diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))


(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; HACK Fixes #1929: icons break file renaming in Emacs 27+, because the icon
  ;;      is considered part of the filename, so we disable icons while we're in
  ;;      wdired-mode.
  (when EMACS27+
    (defvar +wdired-icons-enabled -1)

    (defadvice! +dired-disable-icons-in-wdired-mode-a (&rest _)
      :before #'+wdired-before-start-advice
      (setq-local +wdired-icons-enabled (if all-the-icons-dired-mode 1 -1))
      (when all-the-icons-dired-mode
        (all-the-icons-dired-mode -1)))

    (defadvice! +dired-restore-icons-after-wdired-mode-a (&rest _)
      :after #'+wdired-after-finish-advice
      (all-the-icons-dired-mode +wdired-icons-enabled))))

(use-package! fd-dired
  :when (executable-find doom-projectile-fd-binary)
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))


;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)

(after! wdired
  ;; Temporarily disable `dired-git-info-mode' when entering wdired, due to
  ;; reported incompatibilities.
  (defvar +dired--git-info-p nil)
  (defadvice! +dired--disable-git-info-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq +dired--git-info-p (bound-and-true-p dired-git-info-mode))
    (when +dired--git-info-p
      (dired-git-info-mode -1)))
  (defadvice! +dired--reactivate-git-info-a (&rest _)
    :after '(wdired-exit
             wdired-abort-changes
             wdired-finish-edit)
    (when +dired--git-info-p
      (dired-git-info-mode +1))))
