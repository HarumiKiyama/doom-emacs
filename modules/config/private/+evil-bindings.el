;;; config/default/+bindings.el -*- lexical-binding: t; -*-
;;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                 (and (featurep! :editor snippets)
                      (bound-and-true-p yas-minor-mode)
                      (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                 #'yas-expand
                 (and (featurep! :completion company +tng)
                      (+company-has-completion-p))
                 #'+company/complete)
      :n [tab] (general-predicate-dispatch nil
                 (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 #'+fold/toggle
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)

      :nie "C-s" #'swiper-isearch-thing-at-point

      ;; Smarter newlines
      :i [remap newline] #'newline-and-indent ; auto-indent on newline
      :i "C-j"           #'+default/newline   ; default behavior

      ;; TODO: add escap functions
      ;; emace state binding
      ;; :e [remap esc]            #'evil-normal-state

      ;; workspace based buffer list
      :ni "C-x b" #'switch-to-buffer
      :ni "C-x B" #'+ivy/switch-workspace-buffer

      ;; some binding to magit
      :n "/"  #'magit-status
      :n "gr" #'git-gutter:revert-hunk
      :n "gR" #'vc-revert
      :n "gs" #'git-gutter:stage-hunk
      :n "gS" #'magit-stage
      :n "gt" #'git-timemachine-toggle
      :n "gb" #'magit-blame-addition
      :n "gj" #'git-gutter:next-hunk
      :n "gk" #'git-gutter:previous-hunk
      :n "gc" #'magit-commit-create
      ;; comment function
      :nv "gn"  #'comment-line
      ;; emacs window
      :nie "C-w" evil-window-map

      (:after help :map help-mode-map
        :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
        :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
        :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
        :n "o"       #'link-hint-open-link
        :n "TAB"     #'forward-button
        :n [tab]     #'forward-button
        :n [backtab] #'backward-button)
      (:after view :map view-mode-map
        [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
        :n "q"    #'kill-current-buffer)

      ;; misc
      :n "C-+"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-="    #'text-scale-increase
      :n "C--"    #'text-scale-decrease
      ;; Frame-local font resizing
      :n "M-C-="  #'doom/increase-font-size
      :n "M-C--"  #'doom/decrease-font-size)

;;
;;; Module keybinds

;; window keybindings
(define-key! evil-window-map
  "/" #'evil-window-vsplit
  "-" #'evil-window-split)

;; emacs like insert map
(define-key! evil-insert-state-map
  "C-n" #'next-line
  "C-p" #'previous-line
  "C-d" #'delete-char
  "M-d" #'kill-word
  "C-k" #'kill-line
  "C-y" #'yank
  "C-x C-s" #'save-buffer)

;; normal map
(define-key! evil-normal-state-map
  "s" #'evil-substitute
  "f" #'evil-avy-goto-char-in-line
  "F" #'evil-avy-goto-line
  "t" #'evil-avy-goto-char)

;; Minibuffer
(define-key! evil-ex-completion-map
  "C-a" #'evil-beginning-of-line
  "C-b" #'evil-backward-char
  "C-s" #'counsel-minibuffer-history)


(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
  ;; Scrolling lines
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

;;; :completion
(map! (:after company
          (:map company-active-map
            "C-w"     nil    
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
            "C-s"     #'company-filter-candidates
            "C-s-s"   #'counsel-company
            "C-SPC"   #'company-complete-common
            "TAB"     #'company-complete-common-or-cycle)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            [escape]  #'company-search-abort))
        ;; TAB auto-completion in term buffers
        (:after comint :map comint-mode-map
          "TAB" #'company-complete)

        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "C-l"      #'ivy-done
          [C-return] #'+ivy/git-grep-other-window-action)
)

;;; :ui
(map! (:map +popup-buffer-mode-map
        :n [C-tab]   #'+popup/raise
        :n "q" #'+popup/close)
      :n "C-`"   #'+popup/toggle
      :n "C-x p" #'+popup/other

      :n "C-t"   #'+workspace/new
      :g "M-1"   #'+workspace/switch-to-0
      :g "M-2"   #'+workspace/switch-to-1
      :g "M-3"   #'+workspace/switch-to-2
      :g "M-4"   #'+workspace/switch-to-3
      :g "M-5"   #'+workspace/switch-to-4
      :g "M-6"   #'+workspace/switch-to-5
      :g "M-7"   #'+workspace/switch-to-6
      :g "M-8"   #'+workspace/switch-to-7
      :g "M-9"   #'+workspace/switch-to-8
      :g "M-0"   #'+workspace/switch-to-final
      (:when IS-MAC
        :g "s-t"   #'+workspace/new
        :g "s-T"   #'+workspace/display
        :n "s-1"   #'+workspace/switch-to-0
        :n "s-2"   #'+workspace/switch-to-1
        :n "s-3"   #'+workspace/switch-to-2
        :n "s-4"   #'+workspace/switch-to-3
        :n "s-5"   #'+workspace/switch-to-4
        :n "s-6"   #'+workspace/switch-to-5
        :n "s-7"   #'+workspace/switch-to-6
        :n "s-8"   #'+workspace/switch-to-7
        :n "s-9"   #'+workspace/switch-to-8
        :n "s-0"   #'+workspace/switch-to-final))

;;; :tools
(when (featurep! :tools eval)
  (map! "M-r" #'+eval/buffer))


;;
;;; <leader>

(map! :leader
      :desc "Eval expression"       ";"    #'pp-eval-expression
      :desc "Pop up scratch buffer" "X"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "x"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "Toggle last popup"     "/"    #'+popup/toggle

      :desc "Switch to last buffer" "TAB"    #'evil-switch-to-windows-last-buffer
      :desc "Resume last search"    "'"      #'ivy-resume

      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point

      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;;; <leader> TAB --- workspace
      (:prefix-map ("w" . "workspace")
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "c"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session)

      ;;; <leader> b --- buffer & bookmark
      ;; TODO: add more keybinding
      (:prefix-map ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
        :desc "Kill buffer"                 "d"   #'kill-current-buffer
        :desc "ibuffer"                     "i"   #'ibuffer
        :desc "Set bookmark"                "m"   #'bookmark-set
        :desc "Delete bookmark"             "M"   #'bookmark-delete)

        ;;; <leader> c --- code
        (:prefix-map ("c" . "code")
          :desc "LSP Execute code action"               "a"   #'lsp-execute-code-action
          :desc "Compile"                               "c"   #'compile
          :desc "Recompile"                             "C"   #'recompile
          :desc "Jump to definition"                    "d"   #'+lookup/definition
          :desc "Jump to references"                    "D"   #'+lookup/references
          :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
          :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
          :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
          :desc "LSP Format buffer/region"              "F"   #'+default/lsp-format-region-or-buffer
          :desc "LSP Organize imports"                  "i"   #'lsp-organize-imports
          :desc "Jump to symbol in current workspace"   "j"   #'lsp-ivy-workspace-symbol
          :desc "Jump to symbol in any workspace"       "J"   #'lsp-ivy-global-workspace-symbol
          :desc "Jump to documentation"                 "k"   #'+lookup/documentation
          :desc "LSP Rename"                            "r"   #'lsp-rename
          :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
          :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
          :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
          :desc "List errors"                           "x"   #'flycheck-list-errors)

        ;;; <leader> f --- file
        (:prefix-map ("f" . "file")
          :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
          :desc "Copy this file"              "C"   #'doom/copy-this-file
          :desc "Delete this file"            "D"   #'doom/delete-this-file
          :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
          :desc "Find file from here"         "F"   #'+default/find-file-under-here
          :desc "Recent files"                "r"   #'recentf-open-files
          :desc "Rename/move file"            "R"   #'doom/move-this-file
          :desc "Sudo this file"              "u"   #'doom/sudo-this-file
          :desc "Yank filename"               "y"   #'+default/yank-buffer-filename
          :desc "Browse projects"             "p"   #'+default/browse-projects)

        ;;; <leader> g --- git
        (:prefix-map ("g" . "git")
          :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill-file-or-region
          :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit switch branch"       "b"   #'magit-branch-checkout
          :desc "Magit file delete"         "D"   #'magit-file-delete
          :desc "Magit checkout"            "C"   #'magit-checkout
          :desc "List project tasks"           "t" #'magit-todos-list
          (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
            :desc "Find commit"               "c"   #'magit-show-commit
            :desc "Find issue"                "i"   #'forge-visit-issue
            :desc "Find pull request"         "p"   #'forge-visit-pullreq)
          (:prefix ("o" . "open in browser")
            :desc "Browse file or region"     "o"   #'+vc/browse-at-remote-file-or-region
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)
          (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "C"   #'magit-clone
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Branch"                    "b"   #'magit-branch-and-checkout
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq))

        ;;; <leader> i --- insert
        ;; TODO add more key binding
        (:prefix-map ("i" . "insert")
          :desc "Current file name"             "f"   #'+default/insert-file-path
          :desc "Current file path"             "F"   (λ!! #'+default/insert-file-path t)
          :desc "Snippet"                       "s"   #'yas-insert-snippet
          :desc "From clipboard"                "y"   #'+default/yank-pop)

        ;;; <leader> n --- notes and yas-snippet
        (:prefix-map ("n" . "notes")
          :desc "Find file in notes"            "f" #'+default/find-in-notes
          :desc "Search notes for symbol"       "F" #'+default/search-notes-for-symbol-at-point
          :desc "Search notes"                  "s" #'+default/org-notes-search
          :desc "create snippet"                "c" #'+snippets/new
          :desc "edit snippet"                  "e" #'+snippets/edit
          :desc "view snippets"                 "v" #'+snippets/find)

        ;;; <leader> o --- orgmode
        (:prefix-map ("o" . "open")
          :desc "Org agenda"                    "a" #'org-agenda-list
          :desc "Agenda"                        "A" #'org-agenda
          (:prefix ("j" . "journal")
            :desc "New Entry"                   "j" #'org-journal-new-entry
            :desc "Search Forever"              "s" #'org-journal-search-forever)
          :desc "Pomodoro timer"                "t" #'org-pomodoro
          :desc "Todo list"                     "T" #'org-todo-list
          :desc "Goto org-clock"                "o" #'org-clock-goto
          :desc "Out org-clock"                 "x" #'org-clock-out
          :desc "org-tree-slide mode"           "p" #'+org-present/start
          :desc "Tags search"                   "m" #'org-tags-view
          :desc "View search"                   "v" #'org-search-view)

        ;;; <leader> a -- application
        (:prefix-map ("a" . "app")
          (:when (featurep! :tools docker)
            :desc "Docker" "D" #'docker)
          :desc "Toggle eshell popup"   "e" #'+eshell/toggle
          :desc "Open eshell here"      "E" #'+eshell/here
          :desc "Project sidebar"                     "p" #'+treemacs/toggle
          :desc "Find file in project sidebar"        "P" #'+treemacs/find-file
          :desc "REPL"               "r"  #'+eval/open-repl-other-window
          :desc "Leetcode"           "l"  #'+leetcode/start
          :desc "eww"                "w"  #'eww
          :desc "Start debugger"     "b"  #'+debugger/start
          :desc "Dired"              "d"  #'dired-jump)

        ;;; <leader> p --- project
        (:prefix-map ("p" . "project")
          :desc "Browse project"               "." #'+default/browse-project
          :desc "Browse other project"         ">" #'doom/browse-in-other-project
          :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
          :desc "Add new project"              "a" #'projectile-add-known-project
          :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
          :desc "Compile in project"           "c" #'projectile-compile-project
          :desc "Repeat last command"          "C" #'projectile-repeat-last-command
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Configure project"            "g" #'projectile-configure-project
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find recent project files"    "r" #'projectile-recentf
        :desc "Run project"                  "R" #'projectile-run-project
        :desc "Save project files"           "s" #'projectile-save-project-buffers
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "Test project"                 "T" #'projectile-test-project)

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
        :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
        :desc "Search current directory"     "d" #'+default/search-cwd
        :desc "Search other directory"       "D" #'+default/search-other-cwd
        :desc "Locate file"                  "F" #'locate
        :desc "Imenu"                        "i" #'imenu
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Jump to visible link"         "l" #'link-hint-open-link
        :desc "Jump to link"                 "L" #'ffap-menu
        :desc "Jump list"                    "j" #'evil-show-jumps
        :desc "Look up online"               "o" #'+lookup/online
        :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
        :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
        :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
        :desc "Search project"               "p" #'+default/search-project
        :desc "Search other project"         "P" #'+default/search-other-project
        :desc "Jump to mark"                 "r" #'evil-show-marks
        :desc "find in emcasd"               "e" #'+default/find-in-emacsd
        )

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Flycheck"                     "f" #'flycheck-mode
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Read-only mode"               "r" #'read-only-mode
        :desc "Flyspell"                   "s" #'flyspell-mode))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
