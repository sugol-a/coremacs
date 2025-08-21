;;; -*- lexical-binding: t; -*-

(defvar initial-startup nil)

;; ------------------------------------------------------------
;; "https://github.com/progfolio/elpaca?tab=readme-ov-file#installer"
(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (setq initial-startup t)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)
(add-hook 'elpaca-after-init-hook (lambda () (when initial-startup (restart-emacs))))

(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; ------------------------------------------------------------

(defvar core:dash t
  "If non-nil, enable dash")

(defun core:dash:default-buffer ()
  (let ((buffer (get-buffer-create " *coremacs*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "\n\n > coremacs\n"))
    buffer))

(defvar core:dash:make-buffer #'core:dash:default-buffer
  "Function that returns a buffer to display on startup.")

(defun core:dash-setup ()
  (setq initial-buffer-choice core:dash:make-buffer))

;; ------------------------------------------------------------

(defvar core:font
  nil
  "Default font")

(defun core:ui:-set-font (font)
  "Applies a frame font globally"
  (set-frame-font font nil t))

(defun core:ui:-font-changed (symbol newval operation where)
  (core:ui:-set-font newval))

(defun core:ui-setup ()
  ;; (when core:font
  ;;   (core:ui:-set-font core:font))
  (add-variable-watcher 'core:font #'core:ui:-font-changed))

;; ------------------------------------------------------------

(defvar core:mode-line-buffer-status-token-read-only
  " RO "
  "Token to show when buffer is read-only")

(defvar core:mode-line-buffer-status-token-modified
  " ** "
  "Token to show when buffer is modified")

(defvar core:mode-line-buffer-status-token-read-write
  " RW "
  "Token to show when buffer is writable")

(defface core:mode-line-buffer-status-read-only
  '((t . (:foreground "red")))
  "Face for read-only buffer status")

(defface core:mode-line-buffer-status-modified
  '((t . (:foreground "orange")))
  "Face for modified buffer status")

(defface core:mode-line-buffer-status-read-write
  '((t . (:foreground "blue")))
  "Face for writable buffer status")

(defface core:mode-line-project
  '((t . (:inherit 'mode-line)))
  "Face for project display")

(defface core:mode-line-flycheck-error
  '((t . (:inherit 'error)))
  "Face for flycheck errors")

(defface core:mode-line-flycheck-warning
  '((t . (:inherit 'warning)))
  "Face for flycheck warning errors")

(defface core:mode-line-flycheck-info
  '((t . (:inherit 'mode-line-emphasis)))
  "Face for flycheck information errors")

(defvar-local core:mode-line-buffer-status
    '(:eval (let ((status-display (cond
                                   (buffer-read-only (list core:mode-line-buffer-status-token-read-only
                                                           'core:mode-line-buffer-status-read-only))
                                   ((buffer-modified-p) (list core:mode-line-buffer-status-token-modified
                                                              'core:mode-line-buffer-status-modified))
                                   (t (list core:mode-line-buffer-status-token-read-write
                                            'core:mode-line-buffer-status-read-write)))))
              (propertize (format "%s" (car status-display)) 'face (cdr status-display))))
  "Displays the current buffer status (read-only/modified/writable)")

(defvar-local core:mode-line-project
    '(:eval (when-let* ((project (project-current)))
              (let ((project-string (if-let* ((branch-name (car (vc-git-branches))))
                                        (format "(%s:%s)" (project-name project) branch-name)
                                      (format "(%s)" (project-name project)) 'face 'core:mode-line-project)))
                (propertize project-string 'face 'core:mode-line-project))))
  "Displays the active project and git branch for the current buffer")

(defface core:mode-line-major-mode
  '((t . (:inherit mode-line-emphasis)))
  "Face for mode line major mode")

(defvar-local core:mode-line-major-mode
    '(:eval (propertize (capitalize (if (stringp mode-name)
                                        mode-name
                                      (car mode-name)))
                        'face 'core:mode-line-major-mode)))

(defun core:-mode-line-flycheck-count-errors (kind)
  "Gets the count of KIND erorrs in the flycheck error list"
  (length
   (seq-filter (lambda (elt)
                 (eq kind (flycheck-error-level elt)))
               flycheck-current-errors)))

(defvar-local core:mode-line-flycheck
    '(:eval (when (bound-and-true-p flycheck-current-errors)
              (let ((error-count (core:-mode-line-flycheck-count-errors 'error))
                    (warning-count (core:-mode-line-flycheck-count-errors 'warning))
                    (info-count (core:-mode-line-flycheck-count-errors 'info)))
                (when (or (> error-count 0) (> warning-count 0) (> info-count 0))
                  (list (when (> error-count 0)
                          (propertize (format "%d " error-count) 'face 'core:mode-line-flycheck-error))
                        (when (> warning-count 0)
                          (propertize (format "%d " warning-count) 'face 'core:mode-line-flycheck-warning))
                        (when (> info-count 0)
                          (propertize (format "%d " info-count) 'face 'core:mode-line-flycheck-info)))))))
  "Displays flycheck errors/warnings/info counts for the current buffer")

(defvar core:minimize-frame-function #'iconify-frame)
(defvar core:maximize-frame-function #'toggle-frame-maximized)
(defvar core:close-frame-function #'save-buffers-kill-terminal)

(defvar core:mode-line-window-controls-minimize-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] core:minimize-frame-function)
    (define-key map [mode-line mouse-1] core:minimize-frame-function)
    map))

(defvar core:mode-line-window-controls-maximize-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] core:maximize-frame-function)
    (define-key map [mode-line mouse-1] core:maximize-frame-function)
    map))

(defvar core:mode-line-window-controls-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] core:close-frame-function)
    (define-key map [mode-line mouse-1] core:close-frame-function)
    map))

(defun core:-top-right-window-p (window)
  (let* ((top-windows (window-at-side-list nil 'top))
         (right-windows (window-at-side-list nil 'right))
         (top-right-window (car (seq-filter (lambda (elt) (memq elt right-windows)) top-windows))))
    (eq window top-right-window)))

(defface core:mode-line-window-controls-active
  '((t . (:inherit mode-line)))
  "Face for visible window controls")

(defvar core:mode-line-window-controls-symbols-alist
  '((minimize . "—")
    (maximize . "▢")
    (close . "⤬")))

(defun core:-window-symbol-for (symbol)
  (alist-get symbol core:mode-line-window-controls-symbols-alist))

(defvar-local core:mode-line-window-controls
    '(:eval
      (if (core:-top-right-window-p (get-buffer-window))
          (list
           '(:eval (propertize
                    (core:-window-symbol-for 'minimize)
                    'local-map core:mode-line-window-controls-minimize-map
                    'face 'core:mode-line-window-controls-active))
           " "
           '(:eval (propertize
                    (core:-window-symbol-for 'maximize)
                    'local-map core:mode-line-window-controls-maximize-map
                    'face 'core:mode-line-window-controls-active))
           " "
           '(:eval (propertize
                    (core:-window-symbol-for 'close)
                    'local-map core:mode-line-window-controls-close-map
                    'face 'core:mode-line-window-controls-active)))
        (list
         '(:eval (propertize
                  (core:-window-symbol-for 'minimize)
                  'face `(:inherit core:mode-line-window-controls-active :foreground ,(face-attribute 'mode-line :background))))
           " "
           '(:eval (propertize
                    (core:-window-symbol-for 'maximize)
                    'face `(:inherit core:mode-line-window-controls-active :foreground ,(face-attribute 'mode-line :background))))
           " "
           '(:eval (propertize
                    (core:-window-symbol-for 'close)
                    'face `(:inherit core:mode-line-window-controls-active :foreground ,(face-attribute 'mode-line :background))))))))


(defun core:-line-format-right-align (line-format)
  "`mode-line-format-right-align' tweak, allowing it to work in both the header line and mode line."
  (let* ((rest (cdr (memq 'core:mode-line-format-right-align
			  line-format)))
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

(defun core:mode-line-format-right-align ()
  (core:-line-format-right-align
   (if (eq core:mode-line-position 'top)
       header-line-format
     mode-line-format)))

(defvar-local core:mode-line-format-right-align
    '(:eval (core:mode-line-format-right-align)))

(put 'core:mode-line-format-right-align 'risky-local-variable t)

(defvar core:mode-line-position
  'top
  "Mode line position. Either 'top or 'bottom (nil)")

(defvar core:mode-line t
  "Set to non-nil to enable core:mode-line")

(defvar core:mode-line-format
  (list core:mode-line-buffer-status
        " "
        mode-line-buffer-identification
        " "
        :eval 'core:mode-line-format-right-align
        core:mode-line-major-mode))

(defun core:-mode-line-enable-for-header-line ()
  (setq-default mode-line-format nil)
  (setq-default header-line-format core:mode-line-format))

(defun core:-mode-line-enable-for-mode-line ()
  (setq-default header-line-format nil)
  (setq-default mode-line-format core:mode-line-format))

(defun core:mode-line-setup ()
  (if (eq 'top core:mode-line-position)
      (core:-mode-line-enable-for-header-line)
    (core:-mode-line-enable-for-mode-line)))

;; ------------------------------------------------------------

(defun core:init ()
  "Init coremacs"
  (when core:dash
    (core:dash-setup))
  (when core:mode-line
    (core:mode-line-setup))
  (core:ui-setup))

(provide 'coremacs)
