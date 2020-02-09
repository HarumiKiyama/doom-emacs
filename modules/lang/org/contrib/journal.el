;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-

(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" org-directory)
        org-journal-file-pattern
        (expand-file-name "\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)\\'"
                          org-journal-dir)
        org-journal-date-format "%Y-%m-%d %A")

  (map! :map org-journal-mode-map
        "C-c C-c" (lambda! (save-buffer) (delete-window))
        "C-c n" #'org-journal-open-next-entry
        "C-c p" #'org-journal-open-previous-entry
        :map org-journal-search-mode-map
        "n" #'org-journal-search-next
        "p" #'org-journal-search-prev))
