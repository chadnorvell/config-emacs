;;; core-term.el --- Settings for embedded terminals. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package vterm
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)))

(provide 'core-term)
;;; core-term.el ends here
