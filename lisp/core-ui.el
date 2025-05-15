;;; core-ui.el --- UI and style settings. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(window-divider-mode t)

(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places 'right-only)
(window-divider-mode 1)

(custom-set-faces
 '(window-divider ((t (:inherit vertical-border))))
 '(window-divider-first-pixel ((t (:inherit window-divider))))
 '(window-divider-last-pixel  ((t (:inherit window-divider)))))

(setq window-divider-default-places t
      frame-resize-pixelwise t)

;; Make the fill column indicator a continuous line.
;; Note that the indicator is not displayed by default.
(setq-default display-fill-column-indicator-character ?\s)
(set-face-attribute 'fill-column-indicator nil
		    :background nil
		    :foreground "gray30"
		    :stipple '(7 1 " "))

;; Indent with spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package all-the-icons
  :straight t)

(setq fixed-family "Iosevka")
(setq variable-family "Ubuntu")

;; Set the default (monospace) font via set-frame-font — this preserves fallback support
(set-frame-font (format "%s-14" fixed-family) t t)

;; Reapply fixed-pitch face to match default (used in mixed-pitch)
(set-face-attribute 'fixed-pitch nil :family fixed-family :height 140 :width 'condensed)

;; Set variable-pitch face (used by mixed-pitch and org-mode)
(set-face-attribute 'variable-pitch nil :family variable-family :height 140)

;; Ensure fallback fonts are available for unicode (e.g., box-drawing)
(set-fontset-font t 'unicode "Symbols Nerd Font" nil 'append)

(use-package mixed-pitch
  :straight t
  :hook (org-mode . mixed-pitch-mode))

;; Ensures eldoc can render markdown.
(use-package markdown-mode :straight t)

;; Show eldoc content in a childframe.
(use-package eldoc-box :straight t)
(add-hook 'elgot-managed-mode-hook #'eldoc-box-hover-mode t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package dirvish
  :straight t
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-attributes
   (append
    '(vc-state subtree-state all-the-icons)
    '(git-msg file-size)))
  (dirvish-default-layout '(0 0.4 0.6)))

(use-package posframe
  :straight t)

(use-package hydra-posframe
  :straight (:type git :host github :repo "Ladicle/hydra-posframe")
  :after (hydra posframe)
  :hook (after-init . hydra-posframe-mode)
  :init
  (setq hydra-posframe-border-width 1
        hydra-posframe-poshandler 'posframe-poshandler-window-bottom-center))

(use-package corfu
  :straight t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
	      ("M-SPC"      . corfu-insert-separator)
	      ("TAB"        . corfu-next)
	      ([tab]        . corfu-next)
	      ("S-TAB"      . corfu-previous)
	      ([backtab]    . corfu-previous)
	      ("S-<return>" . corfu-insert)
	      ("RET"        . corfu-insert)))

(provide 'core-ui)
;;; core-ui.el ends here
