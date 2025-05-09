;;; core-projects.el --- Settings for project-related functionality. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package projectile
  :straight t
  :init
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :after projectile
  :init
  (counsel-projectile-mode))

(provide 'core-projects)
;;; core-projects.el ends here
