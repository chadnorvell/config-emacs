;;; mode-org.el --- Settings for org mode. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defvar cxn/home-dir "~"
  "The path string indicating the home directory.")

(defvar cxn/org-root "org"
  "The actual path to the root directory for `org-mode' files.

Evaluated relative to the home directory.

If you use a symlink to your root directory, set this to the target path.")

(defvar cxn/org-root-alias nil
  "The aliased path to the root directory for `org-mode' files.

Evaluated relative to the home directory.

If you use a symlink to your root directory, set this to the link path.")

(defvar cxn/org-root-alias-set? nil
  "Whether the `org-mode' root directory abbreviation has been applied.

This prevents redundant entries being added to `directory-abbrev-alist'")

(defun cxn/should-apply-abbrev-org-root? ()
  "Whether an `org-mode' directory abbreviation should be applied."
  (and cxn/org-root-alias (not cxn/org-root-alias-set?)))

(defun cxn/apply-abbrev-org-root ()
  "Create directory abbreviations of `cxn/org-root-alias' for `cxn/org-root'."
  (if (cxn/should-apply-abbrev-org-root?)
    (push
      (cons
        (concat "^" (file-name-as-directory cxn/home-dir) cxn/org-root)
        (concat (file-name-as-directory cxn/home-dir) cxn/org-root-alias))
      directory-abbrev-alist)
    (setq cxn/org-root-alias-set? t)))

(defun cxn/abbrev-org-path (path)
  "Return the abbreviated path to the `org-mode' file in PATH."
  (replace-regexp-in-string cxn/org-root cxn/org-root-alias path))

(defun cxn/apply-org-file-truename-advice ()
  "Modify `file-truename' to use the abbreviated `org-mode' path."
  (if cxn/org-root-alias
      (advice-add #'file-truename :filter-return #'cxn/abbrev-org-path)))

(defun cxn/org-path (subpath)
  "Return the actual path to an `org-mode' file at SUBPATH."
  (concat
    (file-name-as-directory cxn/home-dir)
    (file-name-as-directory cxn/org-root)
    subpath))

(progn
  (defmacro +org-emphasize (fname char)
    "Make function for setting the emphasis in `org-mode'."
    `(defun ,fname () (interactive)
	    (org-emphasize ,char)))
  
  (+org-emphasize org-bold ?*)
  (+org-emphasize org-code ?~)
  (+org-emphasize org-italic ?/)
  (+org-emphasize org-clear ? )
  (+org-emphasize org-strike-through ?+)
  (+org-emphasize org-underline ?_)
  (+org-emphasize org-verbatim ?=))

(defun set-org-styles ()
  "Set the preferred buffer styles for `org-mode'."
  (setq left-margin-width  4
	    right-margin-width 4)

  (dolist (face '((org-document-title . 1.4)
                  (org-level-1        . 1.2)
                  (org-level-2        . 1.2)))
    (set-face-attribute (car face) nil
		                :weight 'bold
		                :height (cdr face)))

  (dolist (face '((org-level-3        . 1.0)
                  (org-level-4        . 1.0)
                  (org-level-5        . 1.0)
                  (org-level-6        . 1.0)
                  (org-level-7        . 1.0)
                  (org-level-8        . 1.0)))
    (set-face-attribute (car face) nil
		                :weight 'semi-bold
		                :height (cdr face)))

  (setq-default org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(defun cxn/org-insert-subheading ()
  "Insert a new subheading below the current heading."
  (interactive)
  (org-end-of-line)
  (org-insert-heading-respect-content)
  (org-do-demote))

(use-package org
  :init
  (setq
   org-directory                      cxn/org-root-alias
   org-default-notes-file             (cxn/org-path "home.org")
   org-auto-align-tags                nil
   org-catch-invisible-edits          'show-and-error
   org-hide-emphasis-markers          t
   org-pretty-entities                t
   org-special-ctrl-a/e               t
   org-tags-column                    0
   org-cycle-separator-lines          1
   org-indent-indentation-per-level   3
   org-insert-heading-respect-content t
   org-cycle-separator-lines          1
   org-blank-before-new-entry '((plain-list-item . nil)
                                (heading . nil))
   org-todo-keywords '((sequence "TODO(t)"
                                 "PROJ(p)"
                                 "HOLD(h@/!)"
                                 "|"
                                 "DONE(d!)"
                                 "CNCL(c@/!)")))

  :hook
  ;; Ensure text styles are displayed as configured.
  (org-mode . buffer-face-mode)
  ;; Wrap lines and allow operations on virtual lines.
  (org-mode . visual-line-mode)
  ;; Set preferred buffer styles.
  (org-mode . set-org-styles)
  (org-mode . (lambda () (setq-local line-spacing 0.2)))
  ;; If the org dir is a symlink, show files in the symlink root, not the actual root.
  (org-mode . cxn/apply-abbrev-org-root)
  (org-mode . cxn/apply-org-file-truename-advice)

  :general
  ;; Super keybindings
  (general-define-key
   :keymaps 'org-mode-map
   ;; heading manipulation
   "s-<return>"    'org-toggle-heading
   "s-S-<return>"  'cxn/org-insert-subheading
   "s-S-<down>"    'outline-next-visible-heading
   "s-S-<up>"      'outline-previous-visible-heading
   "s-S-<right>"   'org-forward-heading-same-level
   "s-S-<left>"    'org-backward-heading-same-level
   "s-<backspace>" 'outline-up-heading
   "s-n"           'org-narrow-to-subtree
   "s-N"           'widen
   "s-x"           'org-cut-subtree
   "s-r"           'org-refile

   ;; formatting/emphasis
   "s-<space>"     'org-clear
   "s-b"           'org-bold
   "s-i"           'org-italic
   "s-u"           'org-underline
   "s--"           'org-strikethrough
   "s-'"           'org-code
   "s-="           'org-verbatim

   ;; scheduling
   "s-d"           'org-deadline
   "s-D"           'org-schedule
   "s-t"           'org-time-stamp
   "s-T"           'org-time-stamp-inactive)

  ;; <leader>o keybindings
  (cxn/leader-def
    "o"  (cons "org" (make-sparse-keymap))
    "oa" '("agenda"  . org-agenda)
    "oc" '("capture" . counsel-org-capture))

  ;; Major mode keybindings
  (cxn/major-def org-mode-map
    "m" '("edit"                 . hydra-org-edit/body)
    "g" '("go to heading"        . counsel-org-goto)
    "k" '("link to heading"      . counsel-org-link)
    "p" '("set property"         . org-set-property)
    "a" '("attach"               . org-attach)
    "t" '("add tag"              . org-roam-tag-add)
    "T" '("remove tag"           . org-roam-tag-delete)
    "i" '("insert node"          . org-roam-node-insert)
    "f" '("find node"            . org-roam-node-find)
    "d" '("deft"                 . deft)
    "I" '("toggle indent mode"   . org-indent-mode)
    "M" '("toggle inline images" . org-toggle-inline-images)
    "K" '("toggle link display"  . org-toggle-link-display)
    "R" '("roam buffer"          . org-roam-buffer-toggle))

  :config
  (use-package org-capture
    :defer t
    :config
    (setq org-capture-templates
	      `(("i"  "Item" entry (file+datetree ,(cxn/org-path "home.org") "Journal") "*  %?\n")
            ("t"  "Task" entry (file+datetree ,(cxn/org-path "home.org") "Journal") "*  TODO %?\n")

            ("c"  "ctrl")
            ("ci" "Item" entry (file+datetree ,(cxn/org-path "ctrl.org") "Journal") "*  %?\n")
            ("ct" "Task" entry (file+datetree ,(cxn/org-path "ctrl.org") "Journal") "*  TODO %?\n"))))

  (use-package evil-org
    :straight t
    :after evil
    :hook (org-mode . evil-org-mode)
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package org-modern
    :straight t
    :hook (org-mode . org-modern-mode)
    :config
    (custom-set-faces
     '(org-modern-symbol ((t :inherit fixed-pitch))))
    (setq org-modern-star 'replace
	  org-modern-replace-stars (concat " " " " "◉" "○" "›" "»" "⁖" "⁘" "⁙" "•"))))

(provide 'mode-org)
;;; mode-org.el ends here
