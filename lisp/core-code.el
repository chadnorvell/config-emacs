;;; core-code.el --- Settings for code-related functionality. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package magit
  :straight t
  :general
  (cxn/leader-def
    "g"   (cons "git" (make-sparse-keymap))
    "gs"    '("status"       . magit-status)
    "gb"    '("blame"        . magit-blame)
    "gd"    '("diff"         . magit-diff)
    "g+"    '("stage file"   . magit-stage-file)
    "g-"    '("unstage file" . magit-unstage-file)))

(use-package treesit
  :demand t)

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-diagnostics-mode)
	 (lsp-mode . lsp-enable-which-key-integration))

  :general
  (cxn/leader-def
    "c"  (cons "code" (make-sparse-keymap)))

  :bind-keymap ("s-p" . lsp-command-map)
 
  :custom
  (lsp-keymap-prefix "s-p")

  (lsp-diagnositics-provider :flycheck)
  (lsp-idle-delay 0.5)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))

  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-imenu t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-xref t)

  (lsp-enable-file-watchers nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-keep-workspace-alive nil)
  (lsp-lens-enable nil)
  (lsp-log-io nil)

  ;; Disable things provided by other packages
  (lsp-completion-provider :none)
  (lsp-enable-indentation nil)
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-semantic-tokens-enable nil)

  ;; Completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)

  ;; Side line
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
 
  ;; Header line
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
 
  ;; Mode line
  (lsp-eldoc-render-all nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-use-childframe t))

 (use-package lsp-completion
    :no-require
    :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
    :straight t
    :commands
    (lsp-ui-doc-show
    lsp-ui-doc-glance)
    :after (lsp-mode evil)
    :custom
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-show-with-cursor nil)
    (lsp-ui-doc-position 'at-point))

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :config
  (defun cxn/toggle-flycheck-mode ()
    "Toggle Flycheck mode in the current buffer."
    (interactive)
    (if flycheck-mode
        (progn
            (flycheck-mode -1)
            (message "Flycheck disabled in this buffer"))
        (flycheck-mode 1)
        (message "Flycheck enabled in this buffer")))

    (defun cxn/toggle-flycheck-buffer ()
    "Toggle the Flycheck error list buffer."
    (interactive)
    (if (get-buffer-window flycheck-error-list-buffer)
        (quit-windows-on flycheck-error-list-buffer)
    (flycheck-list-errors)))

  :general
  (cxn/leader-def
    "cf" '("toggle flycheck buffer" . cxn/toggle-flycheck-buffer)
    "cF" '("toggle flycheck mode" . cxn/toggle-flycheck-mode)))

(use-package apheleia
  :straight t
  :general
  (general-define-key
    "s-;" 'apheleia-format-buffer)

  (cxn/leader-def
    "c=" '("format buffer +lsp" . lsp-format-buffer)
    "c;" '("format buffer +ext" . apheleia-format-buffer)
    "cA" '("enable auto format" . apheleia-mode)))

(provide 'core-code)
;;; core-code.el ends here
