;;; tools/leetcode/config.el -*- lexical-binding: t; -*-

;; packages
(load! "leetcode.el")

(add-to-list 'evil-overriding-maps '(leetcode--problems-mode))
(add-to-list 'evil-intercept-maps '(leetcode--problems-mode-map))

;; (define-key! leetcode--problems-mode-map
;;   "RET" #'+leetcode/problem-detail
;;   "s" #'leetcode-set-filter-regex
;;   "t" #'leetcode-set-filter-tag
;;   "g" #'leetcode-refresh
;;   "q" #'quit-window)

;; (define-key! leetcode--problems-mode-map
;;       "G" #'leetcode-refresh-fetch
;;       "/" #'leetcode-reset-filter
;;
;;       )
