;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)
;; (global-set-key "\C-l" 'goto-line) ;; M-g g

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont indent the braces
(setq c-default-style "linux"
      c-basic-offset 3)

;; add cuda syntax highlighting
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; add regular c++ files for syntaxhiglighting
;;(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; add template file highlighting
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; latex-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; latex dict
(add-hook 'latex-mode-hook 'flyspell-mode)
;;(setq flyspell-issue-welcome-flag nil) ;; fix flyspell problem
;;(setq flyspell-issue-message-flag nil)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Icons
(require-package 'all-the-icons)
;; run this command: M-x all-the-icons-install-fonts

(require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
;;(setq neo-theme 'icons)
;;(setq neo-theme (if (display-graphic-p) 'ascii))
(setq neo-theme 'ascii)
(setq neo-window-width 35)

(provide 'init-local)
