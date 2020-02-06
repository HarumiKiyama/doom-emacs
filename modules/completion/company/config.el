;;; completion/company/config.el -*- lexical-binding: t; -*-

(use-package! company
  :commands company-complete-common company-manual-begin company-grab-line
  :after-call pre-command-hook after-find-file
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf
                           company-dabbrev
                           company-dabbrev-code)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :config
  (add-hook 'company-mode-hook #'evil-normalize-keymaps)
  ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
  ;; by C-x C-n, will switch from `company-yasnippet' to
  ;; `company-dabbrev-code'.
  (defadvice! +company--abort-previous-a (&rest _)
    :before #'company-begin-backend
    (company-abort))

  (add-hook 'company-mode-hook #'+company-init-backends-h)
  (global-company-mode +1))

;;
;; Packages

(after! company-files
  (pushnew! company-files--regexps
            "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))


(use-package! company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  ;; NOTE prescient config duplicated with `ivy'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))


(use-package! company-dict
  :defer t
  :config
  (setq company-dict-dir (expand-file-name "dicts" doom-private-dir))
  (add-hook! 'doom-project-hook
    (defun +company-enable-project-dicts-h (mode &rest _)
      "Enable per-project dictionaries."
      (if (symbol-value mode)
          (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
        (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))))
