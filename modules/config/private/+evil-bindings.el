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
      :ni "C-s" #'swiper-isearch-thing-at-point

      ;; Smarter newlines
      :i [remap newline] #'newline-and-indent  ; auto-indent on newline
      :i "C-j"           #'+default/newline    ; default behavior

      ;; workspace based buffer list
      :ni "C-x b" #'+ivy/switch-workspace-buffer
      :ni "C-x B" #'switch-to-buffer

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
  "F" #'evil-avy-goto-char
  "t" #'evil-avy-goto-line)

;; Minibuffer
(define-key! evil-ex-completion-map
  "C-a" #'evil-beginning-of-line
  "C-b" #'evil-backward-char
  "C-s" (if (featurep! :completion ivy)
            #'counsel-minibuffer-history
          #'helm-minibuffer-history))

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
(map!   :i "M-/"    #'+company/dabbrev
        (:after company
          (:map company-active-map
            "C-w"     nil     ; don't interfere with `evil-delete-backward-word'
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
      :n "C-M-t" #'+workspace/display
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
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      :desc "Toggle last popup"     "~"    #'+popup/toggle
      :desc "Find file"             "."    #'find-file

      :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
      :desc "Switch buffer"           "<" #'switch-to-buffer

      :desc "Switch to last buffer" "ESC"    #'evil-switch-to-windows-last-buffer
      :desc "Resume last search"    "'"      #'ivy-resume

      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;;; <leader> TAB --- workspace
      (:prefix-map ("TAB" . "workspace")
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final)

        ;;; <leader> b --- buffer
        (:prefix-map ("b" . "buffer")
          :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
          :desc "Previous buffer"             "["   #'previous-buffer
          :desc "Next buffer"                 "]"   #'next-buffer
          :desc "Switch workspace buffer"     "b" #'persp-switch-to-buffer
          :desc "Switch buffer"               "B" #'switch-to-buffer
          :desc "Kill buffer"                 "d"   #'kill-current-buffer
          :desc "ibuffer"                     "i"   #'ibuffer
          :desc "Kill buffer"                 "k"   #'kill-current-buffer
          :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
          :desc "Set bookmark"                "m"   #'bookmark-set
          :desc "Delete bookmark"             "M"   #'bookmark-delete
          :desc "Next buffer"                 "n"   #'next-buffer
          :desc "New empty buffer"            "N"   #'evil-buffer-new
          :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
          :desc "Previous buffer"             "p"   #'previous-buffer
          :desc "Revert buffer"               "r"   #'revert-buffer
          :desc "Save buffer"                 "s"   #'basic-save-buffer
          :desc "Save all buffers"            "S"   #'evil-write-all
          :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
          :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
          :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
          :desc "Bury buffer"                 "z"   #'bury-buffer
          :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

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
          :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
          :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol
          :desc "Jump to documentation"                 "k"   #'+lookup/documentation
          :desc "LSP Rename"                            "r"   #'lsp-rename
          :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
          :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
          :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
          :desc "List errors"                         "x"   #'flycheck-list-errors
          :desc "List errors"                           "X"   #'flymake-show-diagnostics-buffer
          )

        ;;; <leader> f --- file
        (:prefix-map ("f" . "file")
          :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
          :desc "Copy this file"              "C"   #'doom/copy-this-file
          :desc "Find directory"              "d"   #'dired
          :desc "Delete this file"            "D"   #'doom/delete-this-file
          :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
          :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
          :desc "Find file"                   "f"   #'find-file
          :desc "Find file from here"         "F"   #'+default/find-file-under-here
          :desc "Locate file"                 "l"   #'locate
          :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
          :desc "Browse private config"       "P"   #'doom/open-private-config
          :desc "Recent files"                "r"   #'recentf-open-files
          :desc "Rename/move file"            "R"   #'doom/move-this-file
          :desc "Save file"                   "s"   #'save-buffer
          :desc "Save file as..."             "S"   #'write-file
          :desc "Sudo find file"              "u"   #'doom/sudo-find-file
          :desc "Sudo this file"              "U"   #'doom/sudo-this-file
          :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

        ;;; <leader> g --- git
        (:prefix-map ("g" . "git")
          :desc "Git revert file"             "R"   #'vc-revert
          :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill-file-or-region
          :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit switch branch"       "b"   #'magit-branch-checkout
          :desc "Magit status"              "g"   #'magit-status
          :desc "Magit file delete"         "D"   #'magit-file-delete
          :desc "Magit blame"               "B"   #'magit-blame-addition
          :desc "Magit checkout"            "C"   #'magit-checkout
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
            :desc "Commit"                    "c"   #'magit-commit-create
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
        ;; TODO search notes in notes directory
        (:prefix-map ("n" . "notes")
          :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
          :desc "Find file in notes"           "f" #'+default/find-in-notes
          :desc "Search notes"                 "s" #'+default/org-notes-search
          :desc "create snippet"               "c" #'yas-new-snippet
          :desc "view snippets"                "v" #'yas-visit-snippet-file)

        ;;; <leader> o --- orgmode
        ;; TODO add org file search
        (:prefix-map ("o" . "open")
          :desc "Org agenda"       "a"  #'org-agenda-list
          (:prefix ("A" . "org agenda")
            :desc "Agenda"         "a"  #'org-agenda
            :desc "Todo list"      "t"  #'org-todo-list
            :desc "Tags search"    "m"  #'org-tags-view
            :desc "View search"    "v"  #'org-search-view)
          (:prefix ("j" . "journal")
            :desc "New Entry"      "j" #'org-journal-new-entry
            :desc "Search Forever" "s" #'org-journal-search-forever)
          :desc "Pomodoro timer"             "t" #'org-pomodoro
          :desc "Org capture"                "c" #'org-capture
          :desc "Goto org-clock"             "o" #'org-clock-goto
          :desc "Out org-clock"             "x" #'org-clock-out)

        ;;; <leader> a -- application
        (:prefix-map ("a" . "app")
          (:when (featurep! :tools docker)
            :desc "Docker" "D" #'docker)
          :desc "Toggle eshell popup"   "e" #'+eshell/toggle
          :desc "Open eshell here"      "E" #'+eshell/here
          :desc "Project sidebar" "p" #'+treemacs/toggle
          :desc "Find file in project sidebar" "P" #'+treemacs/find-file
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
        :desc "List project tasks"           "t" #'magit-todos-list
        :desc "Test project"                 "T" #'projectile-test-project)

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Delete frame"                 "f" #'delete-frame
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
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
        :desc "Locate file"                  "f" #'locate
        :desc "Jump to symbol"               "i" #'imenu
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
        :desc "Dictionary"                   "t" #'+lookup/offline-definition
        :desc "find in emcasd"               "e" #'+default/find-in-emacsd
        )

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Flymake"                      "F" #'flymake-mode
        :desc "Flycheck"                     "f" #'flycheck-mode
        (:when (featurep! :ui indent-guides)
          :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "org-tree-slide mode"        "p" #'+org-present/start
        :desc "Read-only mode"               "r" #'read-only-mode
        :desc "Flyspell"                   "s" #'flyspell-mode

        ))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
