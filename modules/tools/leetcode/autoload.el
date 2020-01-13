;;; tools/leetcode/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +leetcode/start()
  (interactive)
  (leetcode--async))


(defun +leetcode/test ()
  (interactive)
  (leetcode-try))


(defun +leetcode/submit ()
  (interactive)
  (leetcode-submit))


(defun +leetcode/problem-detail ()
  (interactive)
  (leetcode-show-current-problem))


(defun +leetcode/quit ()
  (interactive)
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--description-buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--result-buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--testcase-buffer-name))
  (mapc (lambda (x) (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--get-code-buffer-name x))))
        leetcode--problem-titles))
