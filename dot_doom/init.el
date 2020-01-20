;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       company                          ; the ultimate code completion backend
       ivy                               ; a search engine for love and life

       :ui
       doom-dashboard         ; a nifty splash screen for Emacs
       modeline               ; snazzy, Atom-inspired modeline, plus API
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (popup                 ; tame sudden yet inevitable temporary windows
        +defaults)            ; default popup rules
       ;; pretty-code       ; replace bits of code with pretty symbols
       treemacs                      ; a project drawer, like neotree but cooler
       ;; unicode           ; extended unicode support for various languages
       vc-gutter              ; vcs diff in the fringe
       window-select          ; visually switch windows
       workspaces             ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)          ; come to the dark side, we have cookies
       file-templates              ; auto-snippets for empty files
       fold                        ; (nigh) universal code folding
       snippets

       :emacs
       (dired
        +icons)                   ; making dired pretty [functional]
       electric                   ; smarter, keyword-based electric-indent
       ibuffer                    ; interactive buffer management
       vc                         ; version-control and Emacs, sitting in a tree

       :term
       eshell                         ; a consistent, cross-platform shell (WIP)
       :checkers
       syntax
       spell

       :tools
       eval                     ; run code, run (also, repls)
       lookup                   ; helps you navigate your code and documentation
       leetcode                 ; an unoffial leetcode client
       lsp
       magit                            ; a git porcelain for Emacs
       pdf                              ; pdf enhancements

       :lang
       cc               ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;coq               ; proofs-as-programs
       data             ; config/data formats
       emacs-lisp       ; drown in parentheses
       ;;go                ; the hipster dialect
       (haskell +lsp)  ; a language that's lazier than I am
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nix               ; I hereby declare "nix geht mehr!"
       (org                             ; organize your plain life in plain text
        +hugo                           ; use Emacs for hugo blogging
        +brain
        +journal
        ;; +jupyter        ; ipython/jupyter support for babel
        +pandoc                     ; export-with-pandoc support
        +pomodoro                   ; be fruitful with the tomato technique
        +present)                   ; using org-mode for presentations
       python                       ; beautiful is better than ugly
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust   ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       scheme ; a fully conniving family of lisps
       sh     ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;web               ; the tubes

       :email
       ;;(mu4e +gmail)

       :app
       irc                          ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       private)
