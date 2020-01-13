;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(use-package! ace-window
  :unless (featurep! +switch-window)
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))
