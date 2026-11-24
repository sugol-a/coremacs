;;; init.el -*- lexical-binding: t; -*-

(require 'coremacs)

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load private-file nil t)))

;; BASICS
;; ============================================================

(setq custom-file null-device
      backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory)))
      scroll-conservatively 101
      enable-recursive-minibuffers t
      use-short-answers t
      ring-bell-function #'ignore
      window-sides-vertical t           ; popup-term spans the frame, not a column
      frame-resize-pixelwise t
      window-resize-pixelwise t
      kill-region-dwim 'emacs-word      ; C-w with no region kills the last word
      project-vc-extra-root-markers '("package.json" "tsconfig.json"))

(setq-default indent-tabs-mode nil
              display-line-numbers-width 4)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Built-in modes that remember things between sessions / keep buffers fresh.
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(if (fboundp 'vc-auto-revert-mode)
    (vc-auto-revert-mode 1)             ; Emacs 31: revert after git checkout etc.
  (global-auto-revert-mode 1))

(electric-pair-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)
(editorconfig-mode 1)

;; Tree-sitter everywhere a grammar exists, and offer to install missing
;; ones (Emacs 31).  Replaces the old commented-out treesit-auto.
(when (boundp 'treesit-enabled-modes)
  (setopt treesit-enabled-modes t
          treesit-auto-install-grammar 'ask))

;; THEME
;; ============================================================
;; Follows the desktop's light/dark preference via the XDG portal, live.

(require 'dbus)

(defvar al:ui/color-scheme-change-hook nil
  "Functions called with `light' or `dark' when the desktop scheme changes.")

(defun al:ui/scheme-from-id (id)
  ;; 0 = no preference, 1 = prefer dark, 2 = prefer light
  (if (equal id 1) 'dark 'light))

(defun al:ui/get-current-color-scheme ()
  (al:ui/scheme-from-id
   (ignore-errors
     (caar (dbus-call-method :session
                             "org.freedesktop.portal.Desktop"
                             "/org/freedesktop/portal/desktop"
                             "org.freedesktop.portal.Settings"
                             "Read"
                             "org.freedesktop.appearance"
                             "color-scheme")))))

(defun al:ui/-on-setting-changed (namespace key value)
  (when (and (string= namespace "org.freedesktop.appearance")
             (string= key "color-scheme"))
    (run-hook-with-args 'al:ui/color-scheme-change-hook
                        (al:ui/scheme-from-id (car value)))))

(ignore-errors
  (dbus-register-signal :session
                        "org.freedesktop.portal.Desktop"
                        "/org/freedesktop/portal/desktop"
                        "org.freedesktop.portal.Settings"
                        "SettingChanged"
                        #'al:ui/-on-setting-changed))

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor (if (eq (al:ui/get-current-color-scheme) 'light) 'latte 'mocha))
  (load-theme 'catppuccin t)
  (add-hook 'al:ui/color-scheme-change-hook
            (lambda (scheme)
              (catppuccin-load-flavor (if (eq scheme 'light) 'latte 'mocha)))))

;; Faces that depend on the palette.  Lives in `core:theme-faces-functions'
;; so it is re-applied after every flavour switch (load-theme resets any
;; `set-face-attribute' done on themed faces).

(defface al:header-buffer '((t :family "Adwaita Sans" :height 0.9))
  "Header line buffer name.")

(defface al:header-buffer-special '((t :inherit al:header-buffer :weight bold))
  "Header line buffer name for *special* buffers.")

(defface al:header-icon '((t :family "AdwaitaMono Nerd Font"))
  "Header line icons.")

(defface al:header-eglot-on '((t :inherit al:header-icon))
  "Major-mode icon when eglot is managing the buffer.")

(defface al:header-eglot-off '((t :inherit al:header-icon))
  "Major-mode icon when eglot is not running.")

(defun al:theme-faces ()
  (when (custom-theme-enabled-p 'catppuccin)
    (let ((base (catppuccin-color 'base))
          (mantle (catppuccin-color 'mantle))
          (crust (catppuccin-color 'crust)))
      (set-face-attribute 'mode-line nil :underline nil)
      (set-face-attribute 'al:header-buffer-special nil :foreground (catppuccin-color 'sky))
      (set-face-attribute 'al:header-eglot-on nil :foreground (catppuccin-color 'sky))
      (set-face-attribute 'al:header-eglot-off nil :foreground (catppuccin-color 'overlay0))
      (dolist (face '(window-divider window-divider-first-pixel window-divider-last-pixel))
        (set-face-attribute face nil :foreground mantle))
      (set-face-attribute 'internal-border nil :background mantle)
      (set-face-attribute 'core:mode-line-buffer-status-read-only nil
                          :background mantle :foreground (catppuccin-color 'mauve))
      (set-face-attribute 'core:mode-line-buffer-status-modified nil
                          :background mantle :foreground (catppuccin-color 'peach))
      (set-face-attribute 'core:mode-line-buffer-status-read-write nil
                          :background mantle :foreground mantle)
      (set-face-attribute 'tab-line nil :background mantle)
      (set-face-attribute 'tab-line-tab-inactive nil :background mantle)
      (set-face-attribute 'tab-line-tab nil :foreground (catppuccin-color 'sky) :background base)
      (set-face-attribute 'tab-line-tab-special nil :slant 'normal)
      (when (facep 'treemacs-window-background-face)
        (set-face-attribute 'treemacs-window-background-face nil :background mantle)
        (set-face-attribute 'treemacs-hl-line-face nil :background crust)))))

(require 'tab-line)
(add-hook 'core:theme-faces-functions #'al:theme-faces)

(setq-default window-divider-default-places t
              window-divider-default-bottom-width 8
              window-divider-default-right-width 8)
(window-divider-mode 1)

;; ICONS
;; ============================================================

(use-package nerd-icons
  :custom (nerd-icons-font-family "AdwaitaMono Nerd Font"))

(dolist (face '(core:mode-line-buffer-status-modified
                core:mode-line-buffer-status-read-only
                core:mode-line-buffer-status-read-write
                core:mode-line-window-controls-active))
  (set-face-attribute face nil :family "AdwaitaMono Nerd Font" :height 130))

;; HEADER LINE
;; ============================================================
;; Evaluated on every redisplay: keep these cheap (see coremacs.el).

(defun al:toggle-eglot ()
  "Start eglot in this buffer, or shut its server down."
  (interactive)
  (if (bound-and-true-p eglot--managed-mode)
      (eglot-shutdown (eglot-current-server))
    (call-interactively #'eglot)))

(defvar al:header-eglot-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'al:toggle-eglot)
    map))

(defvar-local al:--buffer-icon nil
  "Cache: (MAJOR-MODE . ICON).")

(defun al:buffer-icon ()
  (unless (eq (car al:--buffer-icon) major-mode)
    (setq al:--buffer-icon
          (cons major-mode
                (and (fboundp 'nerd-icons-icon-for-buffer)
                     (ignore-errors (nerd-icons-icon-for-buffer))))))
  (cdr al:--buffer-icon))

(defvar-local al:header-eglot
    '(:eval (when-let* ((icon (al:buffer-icon)))
              (concat " "
                      (propertize icon
                                  'face (list (if (bound-and-true-p eglot--managed-mode)
                                                  'al:header-eglot-on
                                                'al:header-eglot-off)
                                              (get-text-property 0 'face icon))
                                  'display '(raise 0.1)
                                  'mouse-face 'highlight
                                  'help-echo "mouse-1: toggle eglot"
                                  'keymap al:header-eglot-map)
                      " "))))

(defvar-local al:header-buffer-name
    '(:eval (let ((name (string-trim (buffer-name))))
              (if (string-match "\\`\\*\\(.*\\)\\*\\'" name)
                  (propertize (match-string 1 name) 'face 'al:header-buffer-special)
                (propertize name 'face 'al:header-buffer)))))

(defvar-local al:header-project
    '(:eval (when-let* ((info (core:project-info)))
              (concat (propertize (car info) 'face 'core:mode-line-project)
                      (when (cdr info)
                        (concat (propertize " \xe725 " 'face '(al:header-icon shadow))
                                (propertize (cdr info) 'face 'shadow)))))))

(setq-default core:mode-line-position 'top
              core:mode-line-buffer-status-token-read-only "\xf444"
              core:mode-line-buffer-status-token-modified "\xf444"
              core:mode-line-buffer-status-token-read-write "\xf444"
              core:mode-line-window-controls-symbols-alist
              '((minimize . " \xeaba ") (maximize . " \xeab9 ") (close . " \xeab8 "))
              core:mode-line-format
              (list al:header-eglot
                    " "
                    al:header-buffer-name
                    " "
                    ;; hidden-buffer terminals (" *terminal*") get no status dot
                    '(:eval (unless (string-prefix-p " " (buffer-name)) core:mode-line-buffer-status))
                    "  "
                    al:header-project
                    'core:mode-line-format-right-align
                    core:mode-line-flycheck
                    "    "
                    core:mode-line-window-controls
                    " "))

;; COMPLETION
;; ============================================================

(use-package vertico
  :custom (vertico-cycle t)
  :config (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config (marginalia-mode 1))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-c C-s" . consult-line-multi)
   ("C-x b" . consult-buffer)
   ("C-x p b" . consult-project-buffer)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-s r" . consult-ripgrep)
   ("M-y" . consult-yank-pop))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package company
  :hook ((eglot-managed-mode emacs-lisp-mode) . company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2))

;; GIT
;; ============================================================

(use-package transient)                 ; magit wants a newer one than Emacs ships

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :demand t
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; LSP / DIAGNOSTICS / DEBUGGING
;; ============================================================

(use-package eglot
  :ensure nil
  :hook ((jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode
          c-mode-common c-ts-mode c++-ts-mode rust-ts-mode
          php-ts-mode python-ts-mode gdscript-mode)
         . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l e" . eglot)
        ("C-c l i" . eglot-inlay-hints-mode)
        ("C-c l a" . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)                 ; kill servers with their last buffer
  (eglot-events-buffer-config '(:size 0 :format full)) ; don't log every JSON message
  :config
  (defvar al:intelephense-stubs
    ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "FFI" "fileinfo" "filter" "fpm" "ftp" "gd" "gettext" "gmp" "hash" "iconv" "imap" "intl" "json" "ldap" "libxml" "mailparse" "mbstring" "meta" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "Reflection" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "xml" "xmlreader" "xmlrpc" "xmlwriter" "xsl" "Zend OPcache" "zip" "zlib"])

  (add-to-list 'eglot-server-programs
               '((php-ts-mode :language-id "php") . ("intelephense" "--stdio")))

  (setq-default eglot-workspace-configuration
                `(:intelephense (:stubs ,al:intelephense-stubs)))

  ;; vacuum >= 0.16 speaks LSP for OpenAPI yaml
  (when-let* ((vacuum (executable-find "vacuum"))
              (version (car (split-string (shell-command-to-string
                                           (concat (shell-quote-argument vacuum) " version")))))
              ((ignore-errors (version<= "0.16" version))))
    (add-to-list 'eglot-server-programs
                 `((yaml-ts-mode :language-id "yaml") . (,vacuum "language-server")))))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  ;; needs the emacs-lsp-booster binary (cargo install emacs-lsp-booster)
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode 1)))

;; flycheck-eglot swaps flymake for flycheck in eglot buffers by itself.
(use-package flycheck
  :custom (flycheck-display-errors-delay 0.1)
  :bind ("C-c C-x C-l" . flycheck-list-errors))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

(use-package eldoc
  :ensure nil
  :bind ("C-c C-/" . eldoc))

(defun al/dape-config-meson (config)
  "Point CONFIG's :program at the first meson executable target."
  (if-let* ((root (expand-file-name (project-root (project-current))))
            (build (seq-find (lambda (dir) (file-directory-p (file-name-concat root dir)))
                             '("build-debug" "build")))
            (targets (json-parse-string
                      (shell-command-to-string
                       (format "meson introspect %s --targets"
                               (shell-quote-argument (file-name-concat root build))))
                      :array-type 'list))
            (exe (seq-find (lambda (target) (string= (gethash "type" target) "executable"))
                           targets)))
      (plist-put config :program (car (gethash "filename" exe)))
    config))

(use-package dape
  :defer t
  :config
  (add-to-list 'dape-configs
               `(xdebug
                 modes (php-mode php-ts-mode)
                 ensure (lambda (config)
                          (dape-ensure-command config)
                          (let ((server (car (plist-get config 'command-args))))
                            (unless (file-exists-p server)
                              (user-error "File %S does not exist" server))))
                 prefix-remote "/var/www/html/"
                 fn (lambda (config)
                      (plist-put config 'prefix-local
                                 (expand-file-name (project-root (project-current)))))
                 command "node"
                 command-args (,(file-name-concat dape-adapter-dir
                                                  "php-debug" "extension" "out" "phpDebug.js"))
                 :type "php"
                 :request "launch"
                 :port 9003))

  (add-to-list 'dape-configs
               `(gdb-meson
                 ensure (lambda (config)
                          (dape-ensure-command config)
                          (let* ((default-directory (or (dape-config-get config 'command-cwd)
                                                        default-directory))
                                 (output (shell-command-to-string
                                          (format "%s --version" (dape-config-get config 'command))))
                                 (version (when (string-match "GNU gdb \\(?:(.*) \\)?\\([0-9.]+\\)" output)
                                            (match-string 1 output))))
                            (unless (and version (version<= "14.1" version))
                              (user-error "Requires gdb version >= 14.1"))))
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 command-cwd dape-command-cwd
                 command "gdb"
                 command-args ("--interpreter=dap")
                 fn (al/dape-config-meson)
                 :request "launch"
                 :args []
                 :stopAtBeginningOfMainSubprogram nil)))

;; LANGUAGES
;; ============================================================

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(add-hook 'yaml-ts-mode-hook (lambda () (setq-local tab-width 2)))
(add-hook 'php-ts-mode-hook (lambda () (setq-local comment-continue "  *")))

(use-package web-mode
  :mode ("\\.blade\\.php\\'" . web-mode))

(use-package jtsx
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

(use-package rust-mode)

(use-package gdscript-mode
  :defer t
  :hook (gdscript-mode . (lambda () (indent-tabs-mode 1))))

(use-package terraform-mode :defer t)
(use-package meson-mode :defer t)

(use-package hledger-mode
  :mode "\\.journal\\'"
  :custom (hledger-currency-string "$")
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'hledger-company)))

(defun al:c-mode-setup ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-close 'c-lineup-close-paren)
  (c-set-offset 'case-label 4))

(add-hook 'c-mode-common-hook #'al:c-mode-setup)
(setq-default c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'bsd)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; ORG / WRITING
;; ============================================================

(use-package org
  :ensure nil
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-folded 'fold)
  (org-hide-leading-stars t)
  :config
  (when (boundp 'al:private:org-agenda-files)
    (setq org-agenda-files al:private:org-agenda-files)))

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear" :branch "org-9.7-fixes")
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(defun al:svg-tags-prog-mode ()
  (setq-local svg-tag-tags
              '(("\\W\\(TODO\\):?\\W" (lambda (_) (svg-tag-make "TODO" :face 'warning :inverse t)))
                ("\\W\\(FIXME\\):?\\W" (lambda (_) (svg-tag-make "FIXME" :face 'error :inverse t)))))
  (svg-tag-mode 1))

(defun al:svg-tags-org-mode ()
  (setq-local svg-tag-tags
              '((":[a-zA-Z0-9-]+:" (lambda (item) (svg-tag-make (substring item 1 -1) :face 'org-document-info)))))
  (svg-tag-mode 1))

(use-package svg-tag-mode
  :hook
  ((prog-mode . al:svg-tags-prog-mode)
   (org-mode . al:svg-tags-org-mode)))

(use-package writeroom-mode
  :bind ("C-x C-z" . writeroom-mode)
  :custom
  (writeroom-restore-window-config t)
  (writeroom-fringes-outside-margins nil)
  (writeroom-width 120)
  :config
  (defvar-local al:--writeroom-restore nil)
  (add-to-list 'writeroom-global-effects
               (lambda (arg)
                 (if (= arg 1)
                     (progn
                       (setq al:--writeroom-restore (cons (bound-and-true-p display-line-numbers-mode)
                                                          truncate-lines))
                       (display-line-numbers-mode -1))
                   (display-line-numbers-mode (if (car al:--writeroom-restore) 1 -1))
                   (toggle-truncate-lines (if (cdr al:--writeroom-restore) 1 -1))))))

;; TOOLS
;; ============================================================

(use-package treemacs
  :bind ("C-c o p" . treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :custom
  (treemacs-user-mode-line-format 'none)
  (treemacs-header-line-format 'none)
  (treemacs-is-never-other-window t)
  :config (al:theme-faces))

(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package which-key
  :ensure nil
  :config (which-key-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-c m l" . mc/edit-lines)
   ("C-c m ;" . mc/mark-next-like-this)
   ("C-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package zoom-window
  :commands zoom-window-zoom)

(use-package agent-shell)

(use-package jwt
  :ensure (:host github :repo "sugol-a/jwt.el")
  :commands jwt-console)

;; Terminal emulator for popup-term.  Unlike term.el it answers the
;; capability queries modern shells send (fish 4 hangs 10s without it),
;; does truecolor, and is a lot faster.
(use-package eat
  :defer t
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size (* 1024 1024)))

(use-package popup-term
  :ensure nil
  :bind ("C-," . popup-term-toggle))

;; Bottom side windows for eldoc and flycheck error lists
(dolist (regexp (list (rx bos "*eldoc") (rx bos "*Flycheck errors*" eos)))
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.2))))

;; Flash the cursor line after big jumps, so you don't lose it.
(defun al:pulse-line (&rest _)
  (pulse-momentary-highlight-one-line))

(dolist (command '(other-window recenter-top-bottom consult-line consult-imenu
                   consult-goto-line xref-find-definitions xref-go-back))
  (advice-add command :after #'al:pulse-line))

;; EDITING
;; ============================================================

(defun beginning-of-line-or-text ()
  "Jump to the first non-blank character, or to column 0 if already there."
  (interactive)
  (let ((pos (point)))
    (beginning-of-line-text)
    (when (= pos (point))
      (beginning-of-line))))

(defun comment-descendent-p (node)
  "Return the comment/document node containing NODE, or nil."
  (treesit-parent-until node
                        (lambda (n) (member (treesit-node-type n) '("comment" "document")))
                        t))

(defun continue-multiline-comment (point)
  "Continue a comment at POINT on a new line with `comment-start' or `comment-continue'."
  (interactive "d")
  (if-let* (((treesit-parser-list))
            (comment (comment-descendent-p (treesit-node-at point))))
      (progn
        (newline)
        (insert (if (string= (treesit-node-type comment) "document")
                    comment-continue
                  comment-start)
                comment-padding)
        (indent-for-tab-command))
    (default-indent-new-line)))

(defun insert-doc-comment ()
  "Insert (or jump to) a /** doc comment above the defun at point."
  (interactive)
  (when (treesit-parser-list)
    (treesit-beginning-of-defun)
    (beginning-of-line)
    (if-let* ((comment (and (not (bobp))
                            (save-excursion
                              (forward-line -1)
                              (back-to-indentation)
                              (comment-descendent-p (treesit-node-at (point)))))))
        (goto-char (treesit-node-start comment))
      (let ((start (point)))
        (insert "/**\n *\n */\n")
        (indent-region start (point))
        (goto-char start)
        (forward-line 1)
        (end-of-line)
        (insert comment-padding)))))

(defun al/move-line (n)
  "Move the current line N lines down (up if N is negative)."
  (let ((col (current-column)))
    (if (> n 0)
        (dotimes (_ n) (forward-line 1) (transpose-lines 1) (forward-line -1))
      (dotimes (_ (- n))
        (unless (= (line-beginning-position) (point-min)) (transpose-lines 1) (forward-line -2))))
    (move-to-column col)))

(defun al/move-region (n)
  "Move the lines spanned by the region N lines down (up if N is negative)."
  (let* ((beg (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
         (end (save-excursion (goto-char (region-end))
                              (if (bolp) (point) (line-beginning-position 2))))
         (point-offset (- (point) beg))
         (mark-offset (- (mark) beg))
         (text (delete-and-extract-region beg end)))
    (forward-line n)
    (let ((start (point)))
      (insert text)
      (set-mark (+ start mark-offset))
      (goto-char (+ start point-offset))
      (setq deactivate-mark nil))))

(defun al/move-thing-down (n)
  (interactive "p")
  (if (use-region-p) (al/move-region n) (al/move-line n)))

(defun al/move-thing-up (n)
  (interactive "p")
  (if (use-region-p) (al/move-region (- n)) (al/move-line (- n))))

(keymap-global-set "C-c d" #'duplicate-dwim)
(keymap-global-set "M-2" #'split-window-below)
(keymap-global-set "M-3" #'split-window-right)
(keymap-global-set "M-0" #'delete-window)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-n" #'al/move-thing-down)
(keymap-global-set "M-p" #'al/move-thing-up)
(keymap-global-set "C-a" #'beginning-of-line-or-text)
(keymap-global-set "C-c h f" #'toggle-frame-fullscreen)
(keymap-set prog-mode-map "M-RET" #'continue-multiline-comment)
(keymap-set prog-mode-map "C-M-;" #'insert-doc-comment)

(core:init)
