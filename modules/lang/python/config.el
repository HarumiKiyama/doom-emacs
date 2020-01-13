;;; lang/python/config.el -*- lexical-binding: t; -*-
;; +python-pyenv-mode-set-auto-h

(defvar +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info")
  "CLI arguments to initialize ipython with when `+python/open-ipython-repl' is
called.")

(defvar +python-jupyter-repl-args '("--simple-prompt")
  "CLI arguments to initialize 'jupiter console %s' with when
`+python/open-ipython-repl' is called.")

(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt"))


;;
;; Packages

(use-package! python
  :defer t
  :init
  (setq python-environment-directory (getenv "WORKON_HOME")
        python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp!)
  :config
  (set-repl-handler! 'python-mode #'+python/open-repl :persist t)
  (set-docsets! 'python-mode "Python 3" "NumPy" "SciPy")
  (set-pretty-symbols! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p))

  ;; Affects pyenv and conda
  (when (featurep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset))

(use-package! pyimport
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Insert missing imports" "i" #'pyimport-insert-missing
          :desc "Remove unused imports"  "r" #'pyimport-remove-unused
          :desc "Optimize imports"       "o" #'+python/optimize-imports)))


(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Sort imports"      "s" #'py-isort-buffer
          :desc "Sort region"       "r" #'py-isort-region)))

(use-package! python-pytest
  :defer t
  :init
  (map! :after python
        :localleader
        :map python-mode-map
        :prefix ("t" . "test")
        "f" #'python-pytest-file-dwim
        "F" #'python-pytest-file
        "t" #'python-pytest-function-dwim
        "T" #'python-pytest-function
        "r" #'python-pytest-repeat
        "p" #'python-pytest-popup))


;;
;; Environment management

(use-package! pyvenv
  :after python
  :init
  (when (featurep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

(use-package! lsp-python-ms
  :after lsp-clients
  :preface
  (after! python
    (setq lsp-python-ms-python-executable-cmd python-shell-interpreter))
  :init
  (defadvice! +python--silence-errors-a (orig-fn &rest args)
    :around #'lsp-python-ms--extra-init-params
    (ignore-errors (apply orig-fn args))))
