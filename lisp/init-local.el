(require-package 'use-package)

;; (global-set-key "\C-l" 'goto-line) ;; M-g g

;; mouse-2 events by pressing Command when clicking with the trackpad.
(when *is-a-mac*
  (define-key key-translation-map (kbd "<S-mouse-1>") (kbd "<mouse-2>")))

;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
;; note M-x delete-trailing-whitespace will delete all trailing whitespace in the buffer.

;; remove trailing whitespace from the entire buffer before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 4) ;; set the tab width
(setq-default indent-tabs-mode nil)  ;; Use spaces instead of tabs for indentation

;; don't hang when visiting files with extremely long lines
(global-so-long-mode t)

;; TODO test later
;; (use-package buffer-move
;;   :defer t
;;   :bind (("M-S-<up>" . buf-move-up)
;;          ("M-S-<down>" . buf-move-down)
;;          ("M-S-<left>" . buf-move-left)
;;          ("M-S-<right>" . buf-move-right))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; font scaling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eldoc-idle-delay 0.1
      eldoc-echo-area-prefer-doc-buffer  t ; Prefer displaying doc in separate buffer
      eldoc-echo-area-use-multiline-p nil)  ; 1 line max to display in the echo area

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :config
  ;; Automatically set `yas-snippet-dirs`
  (setq yas-snippet-dirs
        (append
         '("~/.emacs.d/snippets")  ;; Your custom snippets directory
         (when-let ((snippets-dir
                     (car (file-expand-wildcards "~/.emacs.d/elpa/yasnippet-snippets-*/snippets"))))
           (list snippets-dir))))  ;; Automatically find `yasnippet-snippets`
  (yas-global-mode 1))  ;; Enable globally


(use-package yasnippet-snippets
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; taken from here: https://robbmann.io/emacsd/
(use-package tramp
  :defer t
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq tramp-use-ssh-controlmaster-options nil)

;; add to ~/.ssh/config
;; Host *
;;   ControlMaster auto
;;   ControlPath ~/.ssh/sockets/%C
;;   ControlPersist 10m
;;   ForwardAgent yes
;;   ServerAliveInterval 60
;;   Compression yes

;; can maybe speed up tramp:
;; (setq enable-remote-dir-locals t)
;; (setq tramp-default-method "scp")
;; (setq projectile--mode-line "Projectile")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; python-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set 4-space indent for python
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)
            ;; Customize Tree-sitter highlights
            (setq treesit-font-lock-level 3) ;; Highlighting level
            )
          )

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.conda"))
  (setq conda-env-home-directory (expand-file-name "~/.conda/envs"))
  ;; Optionally enable automatic activation when entering a directory with .conda-env
  (setq conda-env-autoactivate-mode t))

;; to use pyvenv-activate for local venvs
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  ;; (pyvenv-tracking-mode 1)   ;; Enable mode tracking to show venv in mode line
  )

;; taken from https://codeberg.org/haditim/dotemacs/src/branch/master/modules/programming-languages/my-python.el
;; allows to use virtualenvs with all settings
(defun my/reset-python-env ()
  "Resets the env set by my/custom-python-env."
  (when (and (boundp 'orig-exec-path)
             (boundp 'orig-python-interpreter)
             (boundp 'orig-python-shell-interpreter))
    (setq exec-path orig-exec-path
          python-interpreter orig-python-interpreter
          python-shell-interpreter orig-python-shell-interpreter)
    (setenv "PATH" (mapconcat 'identity exec-path ":"))
    (setenv "VIRTUAL_ENV" orig-python-interpreter)
    (setenv "PYTHON_HOME" nil)))

(defun my/custom-python-env ()
  "Set custom shell and env for python (e.g. over Tramp with Conda).
This removes the `poetry' and `pyvenv' dependencies for me."
  ;; (interactive)
  (require 'tramp)
  (let* ((env (expand-file-name (read-directory-name "Python env: ")))
         (bin (concat (expand-file-name (tramp-file-local-name (file-name-as-directory env))) "bin"))
         (interpreter (concat (file-name-as-directory bin) "python"))
         (virt-name (if (tramp-tramp-file-p env)
                        (concat (file-name-nondirectory
                                 (directory-file-name env)) "/t")
                      (file-name-nondirectory
                       (directory-file-name env)))))
    ;; Set original values for later retrieval
    (setq orig-exec-path exec-path
          orig-python-interpreter python-interpreter
          orig-python-shell-interpreter python-shell-interpreter)
    (setq python-interpreter interpreter
          python-shell-interpreter interpreter
          python-shell-virtualenv-root interpreter
          python-shell-virtualenv-path interpreter
          my/venv-name virt-name
          my/venv-address env)
    (add-to-list 'exec-path bin)
    (tramp-cleanup-this-connection)
    (add-to-list 'tramp-remote-path bin)
    (setenv "PATH" (mapconcat 'identity exec-path ":"))
    (setenv "VIRTUAL_ENV" interpreter)
    (setenv "PYTHON_HOME" nil)
    (message "env: %s\nbin: %s\ninterpreter: %s\nCurrent $PATH: %s\nCurrent $VIRTUAL_ENV: %s"
             env bin interpreter (getenv "PATH")(getenv "VIRTUAL_ENV"))))

(define-minor-mode my/python-venv-mode
  "Python venv mode showing info on the modeline"
  :init-value nil
  :global nil
  :lighter " my/pv"
  (if (not my/python-venv-mode)
      (my/reset-python-env)
    (my/custom-python-env)
    (add-to-list 'mode-line-misc-info '(my/python-venv-mode (" venv:" my/venv-name " ")))))

;; (defun my/activate-project-venv ()
;;   "Automatically activate a venv based on the project root."
;;   (when (eq major-mode 'python-mode) ; Only do this for Python files
;;     (let* ((project-root (projectile-project-root))
;;            (venv-path (concat project-root ".venv")))
;;       (when (and project-root (file-directory-p venv-path))
;;         (pyvenv-activate venv-path)))))

;; ;; Add the function to the buffer switch hook
;; (add-hook 'buffer-list-update-hook 'my/activate-project-venv)

;; (require 'flymake-ruff)
;; (add-hook 'python-mode-hook #'flymake-ruff-load)
;; (use-package flymake-ruff
;;   :ensure t)

;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   (defun projectile-pyenv-mode-set ()
;;     "Set pyenv version matching project name."
;;     (let ((project (projectile-project-name)))
;;       (if (member project (pyenv-mode-versions))
;;           (pyenv-mode-set project)
;;         (pyenv-mode-unset))))

;;   (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
;;   (add-hook 'python-mode-hook 'pyenv-mode))

;; this package doesn't show guides on blank lines
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook (python-ts-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-auto-enabled nil)
;;   (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;;   (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-responsive 'top))

(use-package dape
  :ensure t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
   ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the following is taken from here:
  ;; https://codeberg.org/haditim/dotemacs/src/branch/master/modules/programming-languages/my-python.el

  (setq dape-repl-use-shorthand t)

  (defun my/get-real-python-env-path ()
    "Get real python env (Tramp and local) for dape"
    (let ((env (if (boundp 'my/venv-address)
                   my/venv-address
                 python-shell-virtualenv-root)))
      (concat (file-name-as-directory env) "bin")))

  (defun my/debug-over-forwarded-port ()
    "Use dape over a forwarded port with command `ssh -L 1234:localhost:1234 <server-address>'"
    (interactive)
    (let* ((selected-port (read-number "Forwarded port: " 1234))
           (selected-port-str (number-to-string selected-port)))
      (add-to-list 'dape-configs
                   `(debugpy-file-remote-forwarded-port
                     modes (python-mode python-ts-mode)
                     command-cwd my/get-real-python-env-path ; to respect the env I/pyvenv activated
                     command "python"
                     command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" ,selected-port-str)
                     port ,selected-port
                     host "localhost"
                     :program dape-buffer-default
                     :request "launch"
                     :type "python"
                     :console "integratedTerminal"
                     :cwd (lambda () (tramp-file-local-name (file-name-as-directory (dape--default-cwd)))) ; this should be `local' file name in remote
                     ))
      (add-to-list 'dape-configs
                   `(debugpy-module-remote-forwarded-port
                     modes (python-mode python-ts-mode)
                     command-cwd my/get-real-python-env-path ; to respect the env I/pyvenv activated
                     command "python"
                     command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" ,selected-port-str)
                     port ,selected-port
                     host "localhost"
                     :module nil
                     :request "launch"
                     :type "python"
                     :cwd (lambda () (tramp-file-local-name (file-name-as-directory (dape--default-cwd)))) ; this should be `local' file name in remote
                     :console "integratedTerminal"
                     :unittestEnabled t
                     ))
      )
    (dape (dape--read-config)))

  (add-to-list `dape-configs
               '(debugpy-program
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                 port :autoport
                 :program dape-buffer-default
                 :request "launch"
                 :type "python"
                 :cwd (lambda () (tramp-file-local-name (file-name-as-directory (dape--default-cwd)))) ; this should be `local' file name in remote
                 :console "integratedTerminal"
                 ))

  ;; debug pytest
  (add-to-list 'dape-configs
               `(debugpy-pytest
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                 port :autoport
                 :module "pytest"
                 :request "launch"
                 :type "python"
                 :cwd (lambda () (tramp-file-local-name (file-name-as-directory (dape--default-cwd)))) ; this should be `local' file name in remote
                 :console "integratedTerminal"
                 ;;:unittestEnabled t
                 :args ["-s" "-v" "tests/unit_tests/data/dataset_layers/test_image_processing.py::test_tu002_layers"]
                 :stopOnEntry t
                 ))

  ;; import debugpy
  ;; debugpy.listen(('0.0.0.0', 5678))
  ;; print("Waiting for debugger attach...")
  ;; debugpy.wait_for_client()  # This will pause execution until the debugger attaches
  (add-to-list 'dape-configs
               '(debugpy-attach-localhost-5678
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 port 5678
                 host "localhost"
                 :request "attach"
                 :type "python"
                 :console "externalTerminal"
                 :cwd (lambda () (tramp-file-local-name (file-name-as-directory (dape--default-cwd)))) ;; Ensure cwd is local in remote context
                 ))

  (add-to-list 'dape-configs
               '(debugpy-attach-remote-local-code
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 port 5678
                 host "192.168.86.21"
                 :request "attach"
                 :type "python"
                 :console "externalTerminal"
                 :cwd "/home/stefan/Playground/test/" ;; local project path
                 prefix-local "/home/stefan/Playground/test/" ;; local project path
                 prefix-remote "/home/floyd/Playground/test_debugpy/" ;; remote project path (needs to be rsynced with local)
                 ))
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :ensure t
  :config
  (repeat-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont indent the braces
;; (setq c-default-style "linux"
;;       c-basic-offset 3)

;; sets c-ts-mode and c++-ts-mode when using c-mode or c++-mode
;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;; (add-to-list 'major-mode-remap-alist
;;              '(c-or-c++-mode . c-or-c++-ts-mode))

;; add cuda syntax highlighting
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; add regular c++ files for syntaxhiglighting
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; add template file highlighting
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))

;; clang-format
;; NOTE: you need to first install clang-format using apt install
(use-package clang-format
  :ensure t
  :config
  ;; Set a global keybinding for formatting the entire buffer
  (global-set-key [C-M-tab] 'clang-format-region)

  ;; (load "/usr/share/emacs/site-lisp/clang-format-10/clang-format.el")
  ;; This will search for a .clang-format file in your project root folder.
  ;; You can create a config file e.g. using the following command:
  ;; clang-format -style=llvm -dump-config > .clang-format
  (setq clang-format-style "file")
  ;; In case there is no config file clang should use the following fallback
  (setq clang-format-fallback-style "llvm")
  ;;(setq clang-format-fallback-style "Google")
  )

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'clang-format-buffer nil 'make-it-local)))
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'clang-format-buffer nil 'make-it-local)))

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

;; not working [fix later]
;;(require 'init-iwyu)

;; switch from source file to header file or vice versa
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; latex-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; latex dictionary
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
;;(setq flyspell-issue-welcome-flag nil) ;; fix flyspell problem
;;(setq flyspell-issue-message-flag nil)

;; flyspell-correct-word-generic C-c $

;; comment for latex
;;(defun uncomment-region (beg end)
;;  "Uncomment a region of text"
;;  (interactive "r")
;;  (comment-region beg end -1));

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn

    (setq treemacs-follow-after-init          t
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-asc
          treemacs-show-hidden-files          t
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-litter-directories         '("/.venv")
          )

    (treemacs-resize-icons 22)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind

  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dictionary for iSpell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ispell-dictionary "english")

(defun switch-to-english-dict ()
  "Change dictionary to english."
  (interactive)
  (ispell-change-dictionary "english")
  ;; (setq ispell-local-dictionary "english")
  ;; (flyspell-mode 1)
  (flyspell-buffer)
  )

(defun switch-to-german-dict  ()
  "Change dictionary to german."
  (interactive)
  (ispell-change-dictionary "german")
  ;; (setq ispell-local-dictionary "german")
  ;; (flyspell-mode 1)
  (flyspell-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Automatically wrapping when you get to the end of a line
;; note: this is not always a good idea (tables)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; turn on flyspell in org mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Disable confirmation for code block evaluation in Org mode
(setq org-confirm-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; all-the-icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  )

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add spell-checking in comments for all programming language modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (after-load 'flyspell
;;   (define-key flyspell-mode-map (kbd "C-;") nil)
;;   (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; text highlights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gptel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "secrets.el" t t)

;; chatgpt
;; (use-package gptel
;;   :ensure t
;;   :config
;;   ;; use gptel-api-key to set the API key
;;   (setq gptel-api-key chatgpt-api-key)

;;   (setq gptel-model 'gpt-3.5-turbo ;; gpt-4o-mini
;;         gptel-temperature 0.7
;;         gptel-max-tokens 1000
;;         gptel-system-message "You are an Emacs expert assistant."
;;         gptel-buffer-name "*GPT Assistant*")
;;   )

;; Gemini
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gemini-1.5-flash
        gptel-backend (gptel-make-gemini "Gemini"
                        :key gemini-api-key
                        :stream t)
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist
        '((markdown-mode . "# ")
          (org-mode . "* ")
          (text-mode . "# "))
        gptel-directives
        '((programming_tutor . "You are a careful professional programmer. Review the following code and provide concise suggestions to improve it.")
          (programming_explainer . "You are a careful professional programmer. Review the following code and explain concisely how it works.")
          (programming_describer . "You are a careful professional programmer. Review the following code and explain, line by line, how it works.")
          (professional_python_dev . "You are a highly experienced Python developer and mentor specializing in modern Python practices. Write code that:
1. Uses Python 3.10+ features (e.g., `match` statements, type hints, dataclasses).
2. Follows PEP8 standards and Google's Python style guide, ensuring clean and readable code.
3. Strives for high performance without sacrificing readability (e.g., use comprehensions, efficient libraries, lazy evaluation when appropriate).
4. Uses type hints for all function signatures and variables wherever meaningful.
5. Includes concise, professional docstrings with examples when necessary.
6. Handles exceptions gracefully and incorporates robust error handling.
7. Avoids redundant comments; use meaningful variable and function names.
8. Utilizes modern libraries and tools such as `pathlib` for file handling, `functools` for decorators, and `dataclasses` or `attrs` for structured data.
9. Uses expressive but not overly complex one-liners when they enhance clarity.
10. Incorporates Pythonic idioms like unpacking, comprehensions, and context managers.
11. Ensures compatibility and tests edge cases, if applicable.
12. Highlights any trade-offs between readability and performance explicitly.
13. Avoids using deprecated libraries or outdated patterns.
14. Ensures modular design, with functions and classes that are logically separated and reusable.
Respond with clear, professional, and idiomatic Python code that adheres to these principles.")
          )
        )
  )



(provide 'init-local)
