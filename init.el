;; Add lisp subdirectory to load path.
(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; Don't store custom-set-* here.
(setq custom-file (concat user-emacs-directory "/custom.el"))

(require 'locals)
(require 'core-prelude)
(require 'core-functions)
(require 'core-keybindings)
(require 'core-code)
(require 'core-ui)
(require 'core-projects)
(require 'core-term)
(require 'mode-elisp)
(require 'mode-fish)
(require 'mode-org)
