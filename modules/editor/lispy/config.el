;;; editor/lispy/config.el -*- lexical-binding: t; -*-

(use-package! lispy
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (dune-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))
