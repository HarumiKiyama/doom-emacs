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

;;; External resources
(evil-ex-define-cmd "repl"        #'+eval:repl)             ; invoke or send to repl

(evil-ex-define-cmd "sh[ell]"     #'+eshell:run)
(evil-ex-define-cmd "pad"         #'+evil:open-scratch-buffer)

;;; Dealing with buffers
(evil-ex-define-cmd "k[ill]all"   #'+evil:kill-all-buffers)
(evil-ex-define-cmd "k[ill]m"     #'+evil:kill-matching-buffers)
(evil-ex-define-cmd "k[ill]o"     #'doom/kill-other-buffers)
(evil-ex-define-cmd "k[ill]b"     #'doom/kill-buried-buffers)
(evil-ex-define-cmd "l[ast]"      #'doom/popup-restore)

;;; Project navigation
(evil-ex-define-cmd "a"           #'projectile-find-other-file)

(evil-define-command +evil:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper-isearch search))
(evil-ex-define-cmd "sw[iper]" #'+evil:swiper)

(cond ((featurep! :completion ivy)
       (evil-ex-define-cmd "pg[rep]"   #'+ivy:project-search)
       (evil-ex-define-cmd "pg[grep]d" #'+ivy:project-search-from-cwd))

      ((featurep! :completion helm)
       (evil-ex-define-cmd "pg[rep]"   #'+helm:project-search)
       (evil-ex-define-cmd "pg[grep]d" #'+helm:project-search-from-cwd)))

;;; Sessions/tabs
(evil-ex-define-cmd "sclear"      #'+workspace/kill-session)
(evil-ex-define-cmd "sl[oad]"     #'doom/quickload-session)
(evil-ex-define-cmd "ss[ave]"     #'doom/quicksave-session)
(evil-ex-define-cmd "tabc[lose]"  #'+workspace:delete)
(evil-ex-define-cmd "tabclear"    #'doom/kill-all-buffers)
(evil-ex-define-cmd "tabl[ast]"   #'+workspace/switch-to-last)
(evil-ex-define-cmd "tabload"     #'+workspace:load)
(evil-ex-define-cmd "tabn[ew]"    #'+workspace:new)
(evil-ex-define-cmd "tabn[ext]"   #'+workspace:switch-next)
(evil-ex-define-cmd "tabp[rev]"   #'+workspace:switch-previous)
(evil-ex-define-cmd "tabr[ename]" #'+workspace:rename)
(evil-ex-define-cmd "tabs"        #'+workspace/display)
(evil-ex-define-cmd "tabsave"     #'+workspace:save)
