;;; editor/evil/config.el -*- lexical-binding: t; -*-

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.


(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")

(defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode t)

(use-package! evil
  :hook (doom-init-modules . evil-mode)
  :demand t
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(bar +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor '(box "grey")
        ;; must be set before evil/evil-collection is loaded
        evil-want-keybinding nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        ;; TODO Revert this to `selected-windows' once emacs-evil/evil#1233 is
        ;;      resolved, otherwise we get no highlights on */#
        evil-ex-interactive-search-highlight 'all-windows)

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! 'magit-mode-hook evil-ex-hl-update-delay 0.2)
  (setq-hook! 'so-long-minor-mode-hook evil-ex-hl-update-delay 0.25)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Allows you to click buttons without initiating a selection
  (define-key evil-motion-state-map [down-mouse-1] nil)

  ;; Done in a hook to ensure the popup rules load as late as possible
  (add-hook! 'doom-init-modules-hook
    (defun +evil--init-popup-rules-h ()
      (set-popup-rules!
        '(("^\\*evil-registers" :size 0.3)
          ("^\\*Command Line"   :size 8)))))

  ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar +evil--default-cursor-color "#ffffff")
  (defvar +evil--emacs-cursor-color "#ff9999")

  (add-hook! 'doom-load-theme-hook
    (defun +evil-update-cursor-color-h ()
      (setq +evil--default-cursor-color (face-background 'cursor)
            +evil--emacs-cursor-color (face-foreground 'warning))))

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color +evil--emacs-cursor-color))

  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)


  ;; --- keybind fixes ----------------------
  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (add-hook! 'doom-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t)))


  ;; --- evil hacks -------------------------
  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (doom-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  ;; 'gq' moves the cursor to the beginning of selection. Disable this, since
  ;; it's more disruptive than helpful.
  (defadvice! +evil--dont-move-cursor-a (orig-fn &rest args)
    :around #'evil-indent
    (save-excursion (apply orig-fn args)))

  ;; In evil, registers 2-9 are buffer-local. In vim, they're global, so...
  (defadvice! +evil--make-numbered-markers-global-a (_arg)
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil-resolve-vim-path-a)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments')
  (advice-add #'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add #'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

  ;; Fix backspace/DEL commands not respecting `delete-selection-mode',
  ;; smartparens pairs (in some cases), and ignoring
  ;; `+default--delete-backward-char-a' on `delete-char-backward'
  (defadvice! +evil-delete-region-if-mark-a (orig-fn &rest args)
    :override #'evil-delete-backward-char-and-join
    (if (or evil-backspace-join-lines (not (bolp)))
        (call-interactively #'backward-delete-char-untabify)
      (user-error "Beginning of line")))

  ;; Recenter screen after most searches
  (dolist (fn '(evil-visualstar/begin-search-forward
                evil-visualstar/begin-search-backward
                evil-ex-search-word-backward
                evil-ex-search-word-backward
                evil-ex-search-forward
                evil-ex-search-backward))
    (advice-add fn :after #'doom-recenter-a))

  ;; --- custom interactive codes -----------
  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type regexp-match
    :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))
  (evil-ex-define-argument-type regexp-global-match
    :runner +evil-ex-regexp-match)

  (defun +evil--regexp-match-args (arg)
    (when (evil-ex-p)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (list arg (string-to-list flags)))))

  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg regexp-match
    (+evil--regexp-match-args evil-ex-argument))

  (evil-define-interactive-code "<//!>"
    :ex-arg regexp-global-match
    (+evil--regexp-match-args evil-ex-argument))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

  ;; Lazy load evil ex commands
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex))
  (after! evil-ex (load! "+commands")))


;;
;;; Packages

(use-package! evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode)
         . +evil-embrace-lisp-mode-hook-h)
  :hook ((org-mode LaTeX-mode) . +evil-embrace-latex-mode-hook-h)
  :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :init
  (after! evil-surround
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil)

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil-embrace-lisp-mode-hook-h ()
    ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
    ;; `f' rule, which we want for other modes
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
    (embrace-add-pair ?> "<" ">"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))


(use-package! evil-escape
  :commands evil-escape
  :after-call pre-command-hook
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(use-package! evil-exchange
  :commands evil-exchange
  :config
  (add-hook! 'doom-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t))))


(use-package! evil-quick-diff
  :commands (evil-quick-diff evil-quick-diff-cancel))


(use-package! evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))


(use-package! evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :after-call pre-command-hook
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (pushnew! evil-snipe-disabled-modes 'Info-mode 'calc-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(use-package! evil-traces
  :after evil-ex
  :config
  (pushnew! evil-traces-argument-type-alist
            '(+evil:align . evil-traces-global)
            '(+evil:align-right . evil-traces-global))
  (evil-traces-mode))


;; Allows you to use the selection for * and #
(use-package! evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


;;
;;; Text object plugins

(use-package! exato
  :commands evil-outer-xml-attr evil-inner-xml-attr)


;;
;;; Keybinds

;; `evil-collection'
(when (featurep! +everywhere)
  (unless doom-reloading-p
    (load! "+everywhere"))

  (setq evil-collection-company-use-tng (featurep! :completion company +tng))

  ;; Don't let evil-collection interfere with certain keys
  (appendq! evil-collection-key-blacklist
            (append (when (featurep! :tools lookup)
                      '("gd" "gf" "K"))
                    (when (featurep! :tools eval)
                      '("gr" "gR"))
                    '("[" "]" "gz" "<escape>")))

  (defadvice! +evil-collection-disable-blacklist-a (orig-fn)
    :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
    (let (evil-collection-key-blacklist)
      (apply orig-fn))))

;; Keybinds that have no Emacs+evil analogues (i.e. don't exist):
;;   zq - mark word at point as good word
;;   zw - mark word at point as bad
;;   zu{q,w} - undo last marking
;; Keybinds that evil define:
;;   z= - correct flyspell word at point
;;   ]s - jump to previous spelling error
;;   [s - jump to next spelling error

(map! :v  "@"     #'+evil:apply-macro

      ;; ported from vim-unimpaired
      :n  "]b"    #'next-buffer
      :n  "[b"    #'previous-buffer
      :n  "]f"    #'+evil/next-file
      :n  "[f"    #'+evil/previous-file
      (:when (featurep! :ui vc-gutter)
        :n "]d"   #'git-gutter:next-hunk
        :n "[d"   #'git-gutter:previous-hunk)

      (:after elfeed
        :map elfeed-search-update--force
        :n "gr" #'elfeed-search-update--force
        :n "gR" #'elfeed-search-fetch)

      :nv "z="    #'flyspell-correct-at-point
      ;; custom evil keybinds
      :nv "zn"    #'+evil:narrow-buffer
      :n  "zN"    #'doom/widen-indirectly-narrowed-buffer
      :n  "zx"    #'kill-current-buffer

      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv

      ;; window management (prefix "C-w")
      (:map evil-window-map
        ;; Navigation
        "C-w"     #'ace-window
        ;; Swapping windows
        "H"       #'+evil/window-move-left
        "J"       #'+evil/window-move-down
        "K"       #'+evil/window-move-up
        "L"       #'+evil/window-move-right
        "m"       #'doom/window-maximize-buffer
        ;; Window undo/redo
        "u"       #'winner-undo
        "C-r"     #'winner-redo
        "o"       #'doom/window-enlargen

        ;; remove some useless keybinding
        "h" nil "j" nil "k" nil "l" nil
        "S" nil "s" nil "n" nil "q" nil

        ;; Delete window
        "d"       #'evil-window-delete
        "c"       #'ace-delete-window)

      ;; text objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "c" #'evilnc-inner-comment              #'evilnc-outer-commenter
      :textobj "f" #'+evil:defun-txtobj                #'+evil:defun-txtobj
      :textobj "g" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr

      ;; evil-surround
      :v "s" #'evil-surround-region
      :o "s" #'evil-surround-edit

      ;; Omni-completion
      (:prefix "C-x"
        :i "C-l"    #'+company/whole-lines
        :i "C-k"    #'+company/dict-or-keywords
        :i "C-f"    #'company-files
        :i "C-]"    #'company-etags
        :i "s"      #'company-ispell
        :i "C-s"    #'company-yasnippet
        :i "C-o"    #'company-capf
        :i "C-n"    #'+company/dabbrev
        :i "C-p"    #'+company/dabbrev-code-previous))

;; ban motion state mode globally
(setq evil-motion-state-modes nil)
