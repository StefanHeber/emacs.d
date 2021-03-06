(require-package 'use-package)

;; (global-set-key "\C-l" 'goto-line) ;; M-g g

;; mouse-2 events by pressing Command when clicking with the trackpad.
(when *is-a-mac*
  (define-key key-translation-map (kbd "<S-mouse-1>") (kbd "<mouse-2>")))

;; Highlights the current cursor line
(global-hl-line-mode t)

;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
;; note M-x delete-trailing-whitespace will delete all trailing whitespace in the buffer.

;; remove trailing whitespace from the entire buffer before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; smooth scrolling
(setq
 ;;redisplay-dont-pause t
 scroll-margin 0
 scroll-up-aggressively 0.1
 scroll-down-aggressively 0.1
 ;;scroll-step 1
 scroll-conservatively 1
 scroll-preserve-screen-position t)

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

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; python-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set 4-space indent for python
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook 'pyenv-mode))

;; automatically activate a pyenv environment based on .python-version file
(use-package pyenv-mode-auto
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont indent the braces
;; (setq c-default-style "linux"
;;       c-basic-offset 3)

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
;; you need to first install clang-format using apt install
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)
;; This will search for a .clang-format file in your project root folder.
;; You can create a config file e.g. using the following command:
;; clang-format -style=llvm -dump-config > .clang-format
(setq clang-format-style "file")
;; In case there is no config file clang should use the following fallback
(setq clang-format-fallback-style "llvm")
;;(setq clang-format-fallback-style "Google")

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
;;;; matlab-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add matlab syntax highlighting
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; neotree or speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Icons
;; (require-package 'all-the-icons)
;; ;; run this command: M-x all-the-icons-install-fonts

;; (require-package 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme 'ascii)
;; (setq neo-window-width 35)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn

    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-asc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-resize-icons 22)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind

  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ([f8]        . treemacs)
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
;; Fix to overwrite the 'use-package' theme in emacs26
;; cf init-themes.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-hook 'after-init-hook (lambda () (load-theme 'sanityinc-tomorrow-bright)))
;;(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

(provide 'init-local)
