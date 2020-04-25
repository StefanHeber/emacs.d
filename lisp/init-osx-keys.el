;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*

  ;; make option/alt-key meta
  (setq mac-command-key-is-meta nil)
  ;;(setq mac-command-modifier nil)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)

  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "<mac-command-modifier>-`") 'ns-next-frame)
  (global-set-key (kbd "<mac-command-modifier>-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "<mac-command-modifier>-˙") 'ns-do-hide-others)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "<mac-command-modifier>-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
