;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Prevent built-in Org from playing into the byte-compilation of
;; `org-plus-contrib'.
(when-let (orglib (locate-library "org" nil doom--initial-load-path))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))

;; HACK A necessary hack because org requires a compilation step after being
;;      cloned, and during that compilation a org-version.el is generated with
;;      these two functions, which return the output of a 'git describe ...'
;;      call in the repo's root. Of course, this command won't work in a sparse
;;      clone, and more than that, initiating these compilation step is a
;;      hassle, so...
(add-hook! 'straight-use-package-pre-build-functions
  (defun +org-fix-package-h (package &rest _)
    (when (member package '("org" "org-plus-contrib"))
      (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org"))
        (insert "(fset 'org-release (lambda () \"9.3\"))\n"
                "(fset 'org-git-version #'ignore)\n"
                "(provide 'org-version)\n")))))

;; install cutting-edge version of org-mode
(package! org-plus-contrib)
;; ...And prevent other packages from pulling org; org-plus-contrib satisfies
;; the dependency already: https://github.com/raxod502/straight.el/issues/352
(package! org :recipe (:local-repo nil))

(package! avy)
(package! htmlize)
(package! org-bullets :recipe (:host github :repo "Kaligule/org-bullets"))
(package! org-fancy-priorities)
(package! org-yt :recipe (:host github :repo "TobiasZawada/org-yt"))
(package! ox-clip)
(package! toc-org)
(package! org-cliplink)
(package! org-bookmark-heading)

(package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode"))
(package! org-pdfview)
(package! orgit)
(package! org-brain)
(when (featurep! +dragndrop)
  (package! org-download))
(when (featurep! +gnuplot)
  (package! gnuplot)
  (package! gnuplot-mode))
(when (featurep! +jupyter)
  (package! jupyter))
(package! org-pomodoro)

(package! centered-window
  :recipe (:host github :repo "anler/centered-window-mode"))
(package! org-tree-slide)
(package! org-re-reveal)
(package! org-journal)

;;; Babel
(package! ob-async)
(package! ob-go)
(when (featurep! :lang rest)
  (package! ob-restclient))
(package! ob-rust)

(when (featurep! :lang scala)
  (package! ob-ammonite))

;;; Export
(package! ox-pandoc)
(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t))

(when (featurep! :lang rst)
  (package! ox-rst))
