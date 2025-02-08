(require 'coremacs)

(use-package benchmark-init
  :ensure t
  :config (benchmark-init/install))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file null-device
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backup"))))
      scroll-conservatively 101
      enable-recursive-minibuffers t)

(setq core:font "IBM Plex Mono 14")

;; PACKAGES
;; ============================================================

(require 'eglot)

(use-package eldoc
  :ensure nil
  :hook (prog-mode-hook . eldoc-mode)
  :bind ("C-c C-/" . eldoc))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

(use-package company
  :hook (eglot-managed-mode-hook . company-mode))

(use-package flycheck
  :config
  (setq flycheck-display-errors-delay 0.2)
  :bind ("C-c C-x C-l" . flycheck-list-errors))

(use-package vertico
  :config (vertico-mode))

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

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (when (fboundp 'company-mode)
                                    (company-mode))))

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

;; KEYS
;; ============================================================
(keymap-global-set "C-c d" #'duplicate-line)
(keymap-global-set "M-2" #'split-window-below)
(keymap-global-set "M-3" #'split-window-right)
(keymap-global-set "M-0" #'delete-window)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "C-a" #'beginning-of-line-or-text)
(keymap-global-set "C-c h f" #'toggle-frame-fullscreen)
(keymap-global-set "C-x C-z" #'writeroom-mode)

(keymap-set prog-mode-map "M-RET" #'continue-multiline-comment)
(keymap-set prog-mode-map "C-M-;" #'insert-doc-comment)

;; STUFF
;; ============================================================

(defvar-local al:mode-line-buffer-status
    '(:eval (cond
             (buffer-read-only (propertize " RO " 'face `(:background ,(catppuccin-color 'mauve) :foreground ,(catppuccin-color 'base))))
             ((buffer-modified-p) (propertize " ** " 'face `(:background ,(catppuccin-color 'peach) :foreground ,(catppuccin-color 'base))))
             (t (propertize " RW " 'face `(:background ,(catppuccin-color 'sky) :foreground ,(catppuccin-color 'base)))))))

(defvar-local al:mode-line-buffer-identification
    '(:propertize (:eval (buffer-name)) face mode-line-buffer-id))

(defvar-local al:mode-line-project
    '(:eval (when-let ((project (project-current)))
              (propertize (format "(%s)" (project-name project)) 'face `(:background ,(catppuccin-color 'mantle))))))

(defvar-local al:mode-line-major-mode
    '(:eval (propertize (if (stringp mode-name)
                            mode-name
                          (car mode-name)))))

(defvar-local al:mode-line-eglot
    '(:eval (when (bound-and-true-p eglot--managed-mode)
              "*")))

(defun al:header-line-format-right-align ()
  "`mode-line-format-right-align' tweak, allowing it to work in the header line."
  (let* ((rest (cdr (memq 'al:header-line-format-right-align
			  header-line-format)))
	 (rest-str (format-mode-line `("" ,@rest)))
	 (rest-width (progn
                       (add-face-text-property
                        0 (length rest-str) 'mode-line t rest-str)
                       (string-pixel-width rest-str))))
    (propertize " " 'display
		;; The `right' spec doesn't work on TTY frames
		;; when windows are split horizontally (bug#59620)
		(if (and (display-graphic-p)
                         (not (eq mode-line-right-align-edge 'window)))
		    `(space :align-to (- ,mode-line-right-align-edge
                                         (,rest-width)))
		  `(space :align-to (,(- (window-pixel-width)
                                         (window-scroll-bar-width)
                                         (window-right-divider-width)
                                         (* (or (car (window-margins)) 0)
                                            (frame-char-width))
                                         ;; Manually account for value of
                                         ;; `mode-line-right-align-edge' even
                                         ;; when display is non-graphical
                                         (pcase mode-line-right-align-edge
                                           ('right-margin
                                            (or (cdr (window-margins)) 0))
                                           ('right-fringe
                                            ;; what here?
                                            (or (cadr (window-fringes)) 0))
                                           (_ 0))
                                         rest-width)))))))

(defvar-local al:header-line-format-right-align
  '(:eval (al:header-line-format-right-align)))

(put 'al:header-line-format-right-align 'risky-local-variable t)

(defvar al:mode-line-format
  (list al:mode-line-buffer-status
        " "
        ;; mode-line-buffer-identification
        al:mode-line-buffer-identification
        " "
        al:mode-line-project
        :eval 'al:header-line-format-right-align
        al:mode-line-major-mode
        al:mode-line-eglot
        " "))

(use-package emacs
  :ensure nil
  :config
  (setq ring-bell-function (lambda () nil)) ;stfu
  (setq-default c-ts-mode-indent-offset 4)
  (setq-default c-ts-mode-indent-style 'bsd)
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (global-subword-mode 1)
  (global-hl-line-mode 1)

  (setq-default fringe-mode '(1 . 1))
  (set-fringe-mode '(1 . 1))

  (setq-default display-line-numbers-width 4)
  (fset #'yes-or-no-p #'y-or-n-p)

  (setq-default header-line-format al:mode-line-format
                mode-line-format nil)

  (add-to-list 'eglot-server-programs
               '((php-ts-mode :language-id "php") . ("intelephense" "--stdio" :initializationOptions))))

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
  (interactive "d")
  (if treesit-primary-parser
      (let* ((node (treesit-node-at point)))
        (when-let ((comment-type (comment-descendent-p node)))
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

(dolist (hook '(jtsx-tsx-mode-hook
		jtsx-jsx-mode-hook
		jtsx-typescript-mode-hook
		c-mode-common-hook
                c-ts-mode-hook
                c++-ts-mode-hook
                rust-ts-mode-hook
                php-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))

(add-hook 'eglot-managed-mode-hook (lambda ()
				     (keymap-local-set "C-c l a" #'eglot-code-actions)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*eldoc")
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . right)
              (reusable-frames . visible)
              (window-width   . 0.2)))

(core:init)
