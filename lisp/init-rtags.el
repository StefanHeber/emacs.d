;;; init-rtags.el --- Support for the rtags -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  ;; (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;;
(require-package 'rtags)

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;; (define-key c-mode-base-map (kbd "M-*")
  ;;   (function rtags-location-stack-back))
  (define-key c-mode-base-map (kbd "M-[")
    (function rtags-location-stack-back))
  (define-key c-mode-base-map (kbd "M-]")
    (function rtags-location-stack-forward))
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ;; (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  ;;(setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  ;; add company-rtags
  (when (maybe-require-package 'company-rtags)
    (eval-after-load 'company
      '(add-to-list
        'company-backends 'company-rtags))
    ;; (after-load 'company
    ;;   (add-hook 'c-mode-common-hook
    ;;             (lambda () (sanityinc/local-push-company-backend 'company-rtags))))
    )
  ;;(push 'company-rtags company-backends)
  ;; (global-company-mode)
  ;;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require-package 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
  (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
  (add-hook 'objc-mode-hook #'setup-flycheck-rtags)
  ;; Ivy integration
  (setq rtags-display-result-backend 'ivy)
  ;; start rdm process
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  )

(provide 'init-rtags)
