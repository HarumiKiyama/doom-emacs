;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(defvar dashboard-name "*emacs*"
  "The name to use for the dashboard buffer.")

(defvar +doom-dashboard-functions
  '(doom-dashboard-widget-banner
    doom-dashboard-widget-loaded
    doom-dashboard-widget-shortmenu)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

(defvar +doom-dashboard-banner-padding '(0 . 4)
  "Number of newlines to pad the banner with, above and below, respectively.")

(defvar +doom-dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +doom-dashboard-inhibit-functions ()
  "A list of functions which take no arguments. If any of them return non-nil,
dashboard reloading is inhibited.")

(defvar +doom-dashboard-pwd-policy (getenv "HOME")
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:

  'last-project  the `doom-project-root' of the last open buffer
  'last          the `default-directory' of the last open buffer
  a FUNCTION     a function run with the `default-directory' of the last
                 open buffer, that returns a directory path
  a STRING       a fixed path
  nil            `default-directory' will never change")

(defvar +doom-dashboard-menu-sections
  '(("Reload last session"
     :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
     :when (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir))
     :face (:inherit (doom-dashboard-menu-title bold))
     :action doom/quickload-session)
    ("Open org-agenda"
     :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
     :action org-agenda-list)
    ("Recently opened files"
     :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
     :action bookmark-jump)
    )
  "An alist of menu buttons used by `doom-dashboard-widget-shortmenu'. Each
element is a cons cell (LABEL . PLIST). LABEL is a string to display after the
icon and before the key string.

PLIST can have the following properties:

  :icon FORM
    Uses the return value of FORM as an icon (can be literal string).
  :key STRING
    The keybind displayed next to the button.
  :when FORM
    If FORM returns nil, don't display this button.
  :face FACE
    Displays the icon and text with FACE (a face symbol).
  :action FORM
    Run FORM when the button is pushed.")

;;
(defvar +doom-dashboard--last-cwd nil)
(defvar +doom-dashboard--width 80)
(defvar +doom-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +doom-dashboard--pwd-alist ())
(defvar +doom-dashboard--reload-timer nil)

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;;; Bootstrap

(defun +doom-dashboard-init-h ()
  "Initializes Doom's dashboard."
  (unless noninteractive
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (setq doom-fallback-buffer-name dashboard-name
          initial-buffer-choice #'doom-fallback-buffer)
    (when (equal (buffer-name) "*scratch*")
      (set-window-buffer nil (doom-fallback-buffer))
      (if (daemonp)
          (add-hook 'after-make-frame-functions #'+doom-dashboard-reload-frame-h)
        (+doom-dashboard-reload)))
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-configuration-change-hook #'+doom-dashboard-resize-h)
    (add-hook 'window-size-change-functions #'+doom-dashboard-resize-h)
    (add-hook 'doom-switch-buffer-hook #'+doom-dashboard-reload-maybe-h)
    (add-hook 'delete-frame-functions #'+doom-dashboard-reload-frame-h)
    ;; `persp-mode' integration: update `default-directory' when switching perspectives
    (add-hook 'persp-created-functions #'+doom-dashboard--persp-record-project-h)
    (add-hook 'persp-activated-functions #'+doom-dashboard--persp-detect-project-h)
    (when (daemonp)
      (add-hook 'persp-activated-functions #'+doom-dashboard-reload-maybe-h))
    (add-hook 'persp-before-switch-functions #'+doom-dashboard--persp-record-project-h)))

(add-hook 'doom-init-ui-hook #'+doom-dashboard-init-h)

;;
;;; Faces
(defgroup doom-dashboard nil
  "Manage how doom-dashboard is coloured and themed."
  :prefix "doom-dashboard"
  :group 'doom-themes)

(defface doom-dashboard-banner '((t (:inherit font-lock-comment-face)))
  "Face used for the DOOM banner on the dashboard"
  :group 'doom-dashboard)

(defface doom-dashboard-footer '((t (:inherit font-lock-keyword-face)))
  "Face used for the footer on the dashboard"
  :group 'doom-dashboard)

(defface doom-dashboard-footer-icon '((t (:inherit all-the-icons-green)))
  "Face used for the icon of the footer on the dashboard"
  :group 'doom-dashboard)

(defface doom-dashboard-loaded '((t (:inherit font-lock-comment-face)))
  "Face used for the loaded packages benchmark"
  :group 'doom-dashboard)

(defface doom-dashboard-menu-desc '((t (:inherit font-lock-constant-face)))
  "Face used for the key description of menu widgets on the dashboard"
  :group 'doom-dashboard)

(defface doom-dashboard-menu-title '((t (:inherit font-lock-keyword-face)))
  "Face used for the title of menu widgets on the dashboard"
  :group 'doom-dashboard)

;;
;;; Major mode

(define-derived-mode +doom-dashboard-mode special-mode
  "EMACS"
  "Major mode for dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'+doom-dashboard-reposition-point-h nil t))

(define-key! +doom-dashboard-mode-map
  [left-margin mouse-1]   #'ignore
  [down]    #'forward-button
  [up]      #'backward-button
  [tab]     #'forward-button
  [backtab] #'backward-button

  ;; Evil remaps
  [remap evil-next-line]     #'forward-button
  [remap evil-previous-line] #'backward-button
  [remap evil-next-visual-line]     #'forward-button
  [remap evil-previous-visual-line] #'backward-button
  [remap evil-paste-pop-next] #'forward-button
  [remap evil-paste-pop]      #'backward-button
  [remap evil-delete]         #'ignore
  [remap evil-delete-line]    #'ignore
  [remap evil-insert]         #'ignore
  [remap evil-append]         #'ignore
  [remap evil-replace]        #'ignore
  [remap evil-replace-state]  #'ignore
  [remap evil-change]         #'ignore
  [remap evil-change-line]    #'ignore
  [remap evil-visual-char]    #'ignore
  [remap evil-visual-line]    #'ignore)


;;
;;; Hooks

(defun +doom-dashboard-reposition-point-h ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (setq deactivate-mark t)
    (when (bound-and-true-p evil-local-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (ignore-errors
        (goto-char (point-min))
        (forward-button 1))))

(defun +doom-dashboard-reload-maybe-h (&rest _)
  "Reload the dashboard or its state.

If this isn't a dashboard buffer, move along, but record its `default-directory'
if the buffer is real. See `doom-real-buffer-p' for an explanation for what
'real' means.

If this is the dashboard buffer, reload it completely."
  (cond ((+doom-dashboard-p (current-buffer))
         (let (+doom-dashboard-inhibit-refresh)
           (ignore-errors (+doom-dashboard-reload))))
        ((and (not (file-remote-p default-directory))
              (doom-real-buffer-p (current-buffer)))
         (setq +doom-dashboard--last-cwd default-directory)
         (+doom-dashboard-update-pwd))))

(defun +doom-dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp +doom-dashboard--reload-timer)
    (cancel-timer +doom-dashboard--reload-timer)) ; in case this function is run rapidly
  (setq +doom-dashboard--reload-timer (run-with-timer 0.1 nil #'+doom-dashboard-reload t)))

(defun +doom-dashboard-resize-h (&rest _)
  "Recenter the dashboard, and reset its margins and fringes."
  (let (buffer-list-update-hook
        window-configuration-change-hook
        window-size-change-functions)
    (let ((windows (get-buffer-window-list (doom-fallback-buffer) nil t)))
      (dolist (win windows)
        (set-window-start win 0)
        (set-window-fringes win 0 0)
        (set-window-margins
         win (max 0 (/ (- (window-total-width win) +doom-dashboard--width) 2))))
      (when windows
        (with-current-buffer (doom-fallback-buffer)
          (save-excursion
            (with-silent-modifications
              (goto-char (point-min))
              (delete-region (line-beginning-position)
                             (save-excursion (skip-chars-forward "\n")
                                             (point)))
              (insert (make-string
                       (+ (max 0 (- (/ (window-height (get-buffer-window)) 2)
                                    (round (/ (count-lines (point-min) (point-max))
                                              2))))
                          (car +doom-dashboard-banner-padding))
                       ?\n)))))))))

(defun +doom-dashboard--persp-detect-project-h (&rest _)
  "Check for a `last-project-root' parameter in the perspective, and set the
dashboard's `default-directory' to it if it exists.

This and `+doom-dashboard--persp-record-project-h' provides `persp-mode' integration with
the Doom dashboard. It ensures that the dashboard is always in the correct
project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let (pwd (persp-parameter 'last-project-root))
      (+doom-dashboard-update-pwd pwd))))

(defun +doom-dashboard--persp-record-project-h (&optional persp &rest _)
  "Record the last `doom-project-root' for the current perspective. See
`+doom-dashboard--persp-detect-project-h' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (doom-project-root)
     (if (persp-p persp)
         persp
       (get-current-persp)))))


;;
;;; Library

(defun +doom-dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer dashboard-name)))

(defun +doom-dashboard-update-pwd (&optional pwd)
  "Update `default-directory' in the Doom dashboard buffer. What it is set to is
controlled by `+doom-dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (doom-fallback-buffer)
        (doom-log "Changed dashboard's PWD to %s" pwd)
        (setq-local default-directory pwd))
    (let ((new-pwd (+doom-dashboard--get-pwd)))
      (when (and new-pwd (file-accessible-directory-p new-pwd))
        (+doom-dashboard-update-pwd
         (concat (directory-file-name new-pwd)
                 "/"))))))

(defun +doom-dashboard-reload (&optional force)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not +doom-dashboard-inhibit-refresh)
                 (get-buffer-window (doom-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+doom-dashboard-inhibit-functions)))
            force)
    (with-current-buffer (doom-fallback-buffer)
      (doom-log "Reloading dashboard at %s" (format-time-string "%T"))
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode '+doom-dashboard-mode)
            (+doom-dashboard-mode))
          (erase-buffer)
          (run-hooks '+doom-dashboard-functions)
          (goto-char pt)
          (+doom-dashboard-reposition-point-h))
        (+doom-dashboard-resize-h)
        (+doom-dashboard--persp-detect-project-h)
        (+doom-dashboard-update-pwd)
        (current-buffer)))))

;; helpers
(defun +doom-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +doom-dashboard--get-pwd ()
  (let ((lastcwd +doom-dashboard--last-cwd)
        (policy +doom-dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (let ((cwd default-directory))
             (or (doom-project-root lastcwd)
                 cwd)))
          ((eq policy 'last)
           lastcwd)
          ((warn "`+doom-dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))

;;
;;; Widgets
(defun doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '("=================     ===============     ===============   ========  ========"
            "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
            "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
            "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
            "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
            "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
            "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
            "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
            "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
            "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
            "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
            "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
            "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
            "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
            "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
            "||.=='    _-'                                                     `' |  /==.||"
            "=='    _-'                         E M A C S                          \\/   `=="
            "\\   _-'                                                                `-_   /"
            " `''                                                                      ``'"))
    (when (display-graphic-p)
      (let ((image (create-image (expand-file-name "large.svg" (concat doom-emacs-dir "banners/"))
                    'svg nil :scale 2)))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))
(defun doom-dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"))

(defun doom-dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n")
    (dolist (section +doom-dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (+doom-dashboard--center
            (- +doom-dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'doom-dashboard-menu-title)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action) 'face 'doom-dashboard-menu-desc)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (or (when-let (key (where-is-internal action nil t))
                            (with-temp-buffer
                              (save-excursion (insert (key-description key)))
                              (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                (let ((str (match-string 1)))
                                  (replace-match
                                   (upcase (if (< (length str) 3)
                                               str
                                             (substring str 0 3))))))
                              (propertize (buffer-string) 'face 'doom-dashboard-menu-desc)))
                          ""))))
             "\n"))))))
