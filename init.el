(require 'coremacs)

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file null-device
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backup"))))
      scroll-conservatively 101
      enable-recursive-minibuffers t)

(defvar-local al:mode-line-eglot
    '(:eval (when (bound-and-true-p eglot--managed-mode) "*")))

(setq-default core:font "IBM Plex Mono 14"
              core:mode-line-position 'top
              core:mode-line-format (list core:mode-line-buffer-status
                                          " "
                                          mode-line-buffer-identification
                                          " "
                                          core:mode-line-project
                                          " "
                                          :eval 'core:mode-line-format-right-align
                                          " "
                                          core:mode-line-major-mode
                                          al:mode-line-eglot
                                          "  "
                                          core:mode-line-window-controls
                                          " "))

;; PACKAGES
;; ============================================================

(use-package eglot
  :ensure nil
  :config
  ;; point eglot to intelephense for php
  (add-to-list 'eglot-server-programs
               '((php-ts-mode :language-id "php") . ("intelephense" "--stdio" :initializationOptions)))
  
  ;; Automatically enable eglot for common langs
  (dolist (hook '(jtsx-tsx-mode-hook
		  jtsx-jsx-mode-hook
		  jtsx-typescript-mode-hook
		  c-mode-common-hook
                  c-ts-mode-hook
                  c++-ts-mode-hook
                  rust-ts-mode-hook
                  php-ts-mode-hook))
    (add-hook hook #'eglot-ensure))

  ;; Don't ever use flymake. Prefer flycheck
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (flymake-mode -1)
                                       (flycheck-mode 1)))

  (add-hook 'eglot-managed-mode-hook (lambda ()
				       (keymap-local-set "C-c l a" #'eglot-code-actions))))

(use-package eldoc
  :ensure nil
  :hook (prog-mode-hook . eldoc-mode)
  :bind ("C-c C-/" . eldoc))

(use-package treemacs
  :defer t
  :commands (treemacs)
  :bind ("C-c o p" . treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :config
  (setq treemacs-user-mode-line-format 'none
        treemacs-header-line-format 'none
        treemacs-is-never-other-window t)
  (set-face-attribute 'treemacs-window-background-face nil :background (catppuccin-color 'mantle))
  (set-face-attribute 'treemacs-hl-line-face nil :background (catppuccin-color 'crust)))

(use-package all-the-icons)
(use-package treemacs-all-the-icons
  :after (all-the-icons treemacs)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

;; (use-package solaire-mode
;;   :config
;;   (solaire-global-mode))

(use-package company
  :hook (eglot-managed-mode-hook . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

(use-package flycheck
  :config
  (setq flycheck-display-errors-delay 0.1)
  :bind ("C-c C-x C-l" . flycheck-list-errors))

(use-package vertico
  :config (vertico-mode))

;; (defun al:posframe-size-function (buffer)
;;   (let ((window (get-buffer-window buffer)))
;;     (list
;;      :height (buffer-local-value 'vertico-posframe-height buffer)
;;      :min-height (or (buffer-local-value 'vertico-posframe-min-height buffer)
;;                    (let ((height (+ vertico-count 1)))
;;                      (min height (or (buffer-local-value 'vertico-posframe-height buffer) height))))
;;      :max-width (min 500 (window-size window t t)))))

;; (use-package vertico-posframe
;;   :after (catppuccin-theme)
;;   :ensure (:host "github.com" :repo "tumashu/vertico-posframe" :ref "cfce055")
;;   :config
;;   (vertico-posframe-mode 1)
;;   (setq vertico-posframe-border-width 1
;;         vertico-posframe-poshandler #'posframe-poshandler-window-center
;;         vertico-posframe-size-function #'al:posframe-size-function)
;;   (set-face-attribute 'vertico-posframe-border nil
;;                       :background (catppuccin-color 'lavender)))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config (marginalia-mode 1))

(use-package consult
  :defer t
  :commands (consult-line)
  :bind
  ("C-s" . consult-line)
  ("C-c C-s" . consult-line-multi))

(use-package treesit
  :ensure nil
  :mode ("\\.php\\'" . php-ts-mode))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package jtsx
  :defer t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
	 ("\\.tsx\\'" . jtsx-tsx-mode)
	 ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :custom
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 2)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync nil)
  (jtsx-enable-all-syntax-highlighting-features t))

(use-package rust-mode
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package hledger-mode
  :defer t
  :commands (hledger-mode)
  :config
  (setq hledger-currency-string "$")
  (add-to-list 'company-backends 'hledger-company))

(use-package zoom-window
  :defer t
  :commands (zoom-window-zoom))

(use-package writeroom-mode
  :defer t
  :commands (writeroom-mode)
  :bind ("C-x C-z" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-fringes-outside-margins nil
        writeroom-width 120)
  (add-to-list 'writeroom-global-effects
               (lambda (arg)
                 (cond
                  ((= arg 1) (progn
                               (setq-local restore-line-numbers display-line-numbers)
                               (setq-local restore-truncate-lines truncate-lines)
                               (display-line-numbers-mode -1)))
                  ((= arg -1) (progn (display-line-numbers-mode (if restore-line-numbers
                                                                    1
                                                                  -1))
                                     (toggle-truncate-lines (if restore-truncate-lines
                                                                1
                                                              -1))))))))

(use-package multiple-cursors
  :defer t
  :bind
  ("C-c m l" . #'mc/edit-lines)
  ("C-c m ;" . #'mc/mark-next-like-this))

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (when (fboundp 'company-mode)
                                    (company-mode))))

(use-package org
  :ensure nil
  :config
  (setq org-hide-emphasis-markers t
        org-startup-folded 'fold
        org-hide-leading-stars t)

  (when (boundp 'al:private:org-agenda-files)
    (setq org-agenda-files al:private:org-agenda-files)))

(use-package org-appear
  :ensure (:host "github.com" :repo "awth13/org-appear" :branch "org-9.7-fixes")
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :defer t
  :commands (org-fragtog-mode)
  :hook (org-mode . org-fragtog-mode))

(defun al:svg-tags-prog-mode ()
  (setq-local svg-tag-tags `(("\\W\\(TODO\\):?\\W" (lambda (_) (svg-tag-make "TODO" :face 'warning :inverse t)))
                             ("\\W\\(FIXME\\):?\\W" (lambda (_) (svg-tag-make "FIXME" :face 'error :inverse t)))))
  (svg-tag-mode 1))

(defun al:svg-tags-org-mode ()
  (setq-local svg-tag-tags `((":[a-zA-Z0-9-]+:" (lambda (item) (svg-tag-make (substring item 1 -1) :face 'org-document-info)))))
  (svg-tag-mode 1))

(use-package svg-tag-mode
  :defer t
  :after (catppuccin-theme)
  :hook
  (prog-mode . al:svg-tags-prog-mode)
  (org-mode . al:svg-tags-org-mode))

;; EDIT
;; ============================================================
(defun beginning-of-line-or-text ()
  (interactive)
  (let ((point-current (point))
	(point-bol-text (save-excursion
			  (beginning-of-line-text)
			  (point))))
    (if (eq point-current point-bol-text)
	(beginning-of-line)
      (beginning-of-line-text))))

(add-hook 'php-ts-mode-hook (lambda () (setq-local comment-continue "  *")))

(defun comment-descendent-p (node)
  "Returns t if node is inside a comment"
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= (treesit-node-type node) "document") node)
     ((string= (treesit-node-type node) "comment") node)
     ((eq nil (treesit-node-parent node)) nil)
     (t (comment-descendent-p (treesit-node-parent node))))))

(defun continue-multiline-comment (point)
  "Continues a multiline comment from POINT by inserting a newline and `comment-begin' or `comment-continue'"
  (interactive "d")
  (if treesit-primary-parser
      (let* ((node (treesit-node-at point)))
        (when-let* ((comment-type (comment-descendent-p node)))
          (cond 
           ((string= (treesit-node-type comment-type) "comment")
            (newline)
            (insert comment-start comment-padding)
            (indent-for-tab-command))
           ((string= (treesit-node-type comment-type) "document")
            (newline)
            (insert comment-continue comment-padding)
            (indent-for-tab-command)))))
    (newline-and-indent)
    (comment-dwim)))

(defun insert-doc-comment (point)
  "Inserts a doc comment at the beginning of the defun at point."
  (interactive "d")
  (when treesit-primary-parser
    (treesit-beginning-of-defun)
    (previous-line)
    (beginning-of-line)
    (if-let* ((comment (comment-descendent-p (treesit-node-at (point)))))
        (goto-char (treesit-node-start comment)) ;already have a doc comment, go to it
      (end-of-line)
      (newline-and-indent)
      (insert "/**")
      (newline-and-indent)
      (insert " *")
      (save-excursion
        (newline-and-indent)
        (insert " */"))
      (insert comment-padding))))

;; EMACS
;; ============================================================

(use-package emacs
  :ensure nil
  :after (catppuccin-theme)
  :bind
  ("C-c d" . duplicate-line)
  ("M-2" . split-window-below)
  ("M-3" . split-window-right)
  ("M-0" . delete-window)
  ("M-o" . other-window)
  ("C-a" . beginning-of-line-or-text)
  ("C-c h f" . toggle-frame-fullscreen)
  ("C-," . popup-term-toggle)
  (:map prog-mode-map
        ("M-RET" . continue-multiline-comment)
        ("C-M-;" . insert-doc-comment))

  :config
  (setq ring-bell-function (lambda () nil)) ;stfu

  (setq-default c-ts-mode-indent-offset 4)
  (setq-default c-ts-mode-indent-style 'bsd)

  ;; Needed for popup-term to act in a sane way (ie. don't spawn the
  ;; window beneath side-windows on the left or right of the frame)
  (setq window-sides-vertical t)

  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (global-subword-mode 1)
  (global-hl-line-mode 1)
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode 1)

  ;; Show dividers betweeen windows for better visual separation
  (setq-default window-divider-default-places t
                window-divider-default-bottom-width 8
                window-divider-default-right-width 8)
  (window-divider-mode 1)

  (set-face-attribute 'window-divider nil
                      :foreground (catppuccin-color 'mantle))

  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (catppuccin-color 'surface0))

  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (catppuccin-color 'surface0))

  (set-face-attribute 'internal-border nil
                      :background (catppuccin-color 'mantle))

  (set-face-attribute 'core:mode-line-buffer-status-read-only nil
                      :foreground (catppuccin-color 'base)
                      :background (catppuccin-color 'mauve))
  
  (set-face-attribute 'core:mode-line-buffer-status-modified nil
                      :foreground (catppuccin-color 'base)
                      :background (catppuccin-color 'peach))
  
  (set-face-attribute 'core:mode-line-buffer-status-read-write nil
                      :foreground (catppuccin-color 'base)
                      :background (catppuccin-color 'sky))

  (setq-default display-line-numbers-width 4)
  (fset #'yes-or-no-p #'y-or-n-p)

  ;; Show eldoc and flycheck error lists at the bottom of the frame
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*eldoc")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-width   . 0.2)))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-width   . 0.2)))

  (add-hook 'popup-term-setup-hook (lambda ()
                                     (term-set-escape-char ?\C-x)
                                     (set-window-fringes nil 0 0)
                                     (hl-line-mode -1)))

  (put 'narrow-to-region 'disabled nil))

(defvar popup-term-buffer-name "terminal"
  "Name of the buffer to use for the popup terminal")

(defvar popup-term-shell (getenv "SHELL")
  "Shell to use when opening a popup terminal")

(defvar popup-term-setup-hook nil
  "Hooks to run when setting up a popup terminal")

(defun popup-term ()
  "Displays or switches to a popup terminal running `popup-term-shell'."
  (interactive)
  (let ((term-buffer-name (format "*%s*" popup-term-buffer-name)))
    (if-let* ((term-buffer (get-buffer term-buffer-name))
              (term-window (get-buffer-window term-buffer)))
        (select-window term-window)
      (let* ((term-buffer (get-buffer-create term-buffer-name))
             (term-window (display-buffer-in-side-window term-buffer '((side . bottom) (dedicated . t) (window-parameters . ((no-delete-other-windows . t) (no-other-window . t)))))))
        (with-current-buffer term-buffer
          (make-term popup-term-buffer-name popup-term-shell)
          (term-char-mode)
          (run-hooks 'popup-term-setup-hook))
        (window-preserve-size term-window)
        (select-window term-window)))))

(defun popup-term-hide ()
  "Hides the popup terminal, if it is visible."
  (interactive)
  (when-let* ((term-window (get-buffer-window (get-buffer (format "*%s*" popup-term-buffer-name)))))
    (delete-window term-window)))

(defun popup-term-toggle ()
  "Toggles the popup terminal."
  (interactive)
  (if-let* ((buffer (get-buffer (format "*%s*" popup-term-buffer-name)))
            (window (get-buffer-window buffer)))
      (if (eq window (selected-window))
          (popup-term-hide)
        (popup-term))
    (popup-term)))

(defun c-mode-setup ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-close 'c-lineup-close-paren)
  (c-set-offset 'case-label 4))

(defun prog-mode-setup ()
  (display-line-numbers-mode 1)
  (indent-tabs-mode -1))

(add-hook 'prog-mode-hook #'prog-mode-setup)
(add-hook 'c-mode-common-hook #'c-mode-setup)

(core:init)
