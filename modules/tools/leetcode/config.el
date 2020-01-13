;;; tools/leetcode/config.el -*- lexical-binding: t; -*-

;; packages
(load! "leetcode.el")

(define-key! :states 'normal :keymaps 'leetcode--problems-mode-map
   "RET" #'+leetcode/problem-detail
   "s" #'leetcode-set-filter-regex
   "t" #'leetcode-set-filter-tag
   "g" #'leetcode-refresh
   "/" #'leetcode-reset-filter
   "G" #'leetcode-refresh-fetch)
