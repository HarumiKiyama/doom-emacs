;;; tools/dict/config.el -*- lexical-binding: t; -*-

(set-popup-rule! "^\\*dict" :select t :quit t :size 0.45)

;;;###autoload
(defun +dict/offline-definition (word &optional args)
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))

  (let* ((debug-on-error t)
         (buf (get-buffer-create "*dict*"))
         (command (concat "dict " word))
         (buf-content (shell-command-to-string command)))

    (with-current-buffer buf
      (erase-buffer)
      (insert buf-content)
      (goto-char 1)
      )
    (display-buffer buf)
    )
  )
