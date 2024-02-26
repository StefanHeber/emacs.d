;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
;; (setq read-process-output-max (* 1024 1024))

;; eglot bug??
;; (require 'text-property-search)

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

;; Configure eglot-flymake-backend for C++ files
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-10"))))


(provide 'init-eglot)
;;; init-eglot.el ends here