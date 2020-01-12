;;; lang/python/config.el -*- lexical-binding: t; -*-

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
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)

  (when (featurep! +lsp)
    (add-hook 'python-mode-local-vars-hook #'lsp!))
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

(use-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv"))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))
  (map! :map python-mode-map
        :localleader
        :prefix "e"
        :desc "activate"    "a" #'pipenv-activate
        :desc "deactivate"  "d" #'pipenv-deactivate
        :desc "install"     "i" #'pipenv-install
        :desc "lock"        "l" #'pipenv-lock
        :desc "open module" "o" #'pipenv-open
        :desc "run"         "r" #'pipenv-run
        :desc "shell"       "s" #'pipenv-shell
        :desc "uninstall"   "u" #'pipenv-uninstall))


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



(use-package! pyenv-mode
  :when (featurep! +pyenv)
  :after python
  :config
  (pyenv-mode +1)
  (when (executable-find "pyenv")
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (add-hook 'python-mode-hook #'+python-pyenv-mode-set-auto-h)
  (add-hook 'doom-switch-buffer-hook #'+python-pyenv-mode-set-auto-h))


(use-package! conda
  :when (featurep! +conda)
  :after python
  :config
  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base")
               if (file-directory-p dir)
               return (setq conda-anaconda-home dir
                            conda-env-home-directory dir))
      (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))


(use-package! lsp-python-ms
  :when (featurep! +lsp)
  :after lsp-clients
  :preface
  (after! python
    (setq lsp-python-ms-python-executable-cmd python-shell-interpreter))
  :init
  ;; HACK If you don't have python installed, then opening python buffers with
  ;;      this on causes a "wrong number of arguments: nil 0" error, because of
  ;;      careless usage of `cl-destructuring-bind'. This silences that error,
  ;;      since we may still want to write some python on a system without
  ;;      python installed!
  (defadvice! +python--silence-errors-a (orig-fn &rest args)
    :around #'lsp-python-ms--extra-init-params
    (ignore-errors (apply orig-fn args))))


(use-package! cython-mode
  :when (featurep! +cython)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  (map! :map cython-mode-map
        :localleader
        :prefix "c"
        :desc "Cython compile buffer"    "c" #'cython-compile))


(use-package! flycheck-cython
  :when (featurep! +cython)
  :when (featurep! :tools flycheck)
  :after cython-mode)
