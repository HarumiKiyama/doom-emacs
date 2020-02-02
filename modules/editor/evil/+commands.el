;;; editor/evil/+commands.el -*- lexical-binding: t; -*-

;;
;;; Custom commands
;; Editing
(evil-ex-define-cmd "@"            #'+evil:macro-on-all-lines)   ; TODO Test me
(evil-ex-define-cmd "R[ead]"       #'+evil:read)
(evil-ex-define-cmd "al[ign]"      #'+evil:align)
(evil-ex-define-cmd "ral[ign]"     #'+evil:align-right)
(evil-ex-define-cmd "enhtml"       #'+web:encode-html-entities)
(evil-ex-define-cmd "dehtml"       #'+web:decode-html-entities)
(evil-ex-define-cmd "iedit"        #'evil-multiedit-ex-match)
(evil-ex-define-cmd "na[rrow]"     #'+evil:narrow-buffer)
(evil-ex-define-cmd "retab"        #'+evil:retab)
(evil-ex-define-cmd "rev[erse]"    #'+evil:reverse-lines)
(evil-ex-define-cmd "l[ine]diff"   #'evil-quick-diff)

;;; Dealing with buffers
(evil-ex-define-cmd "k[ill]all"   #'+evil:kill-all-buffers)
(evil-ex-define-cmd "k[ill]m"     #'+evil:kill-matching-buffers)
(evil-ex-define-cmd "k[ill]o"     #'doom/kill-other-buffers)
(evil-ex-define-cmd "k[ill]b"     #'doom/kill-buried-buffers)
(evil-ex-define-cmd "l[ast]"      #'doom/popup-restore)

;;; Project navigation
(evil-ex-define-cmd "a"           #'projectile-find-other-file)

(evil-ex-define-cmd "pg[rep]" #'+ivy:project-search)
(evil-ex-define-cmd "pg[grep]d" #'+ivy:project-search-from-cwd)
