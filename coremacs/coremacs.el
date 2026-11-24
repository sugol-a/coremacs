;;; -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; "https://github.com/progfolio/elpaca?tab=readme-ov-file#installer"
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)

(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  ;; `elpaca-use-package-by-default' was removed from elpaca-use-package
  ;; (commit 98262e4, 2026-02). The supported way to make every `use-package'
  ;; declaration install via Elpaca is `use-package-always-ensure' (Elpaca
  ;; advises the `:ensure' handler). `:ensure nil' still opts a form out.
  (setq use-package-always-ensure t))

;; Block until elpaca-use-package is installed/activated so the `use-package'
;; forms in the init file are expanded with Elpaca's `:ensure' support active.
;; Without this, in an interactive/daemon session (where Elpaca processes its
;; queue asynchronously) those forms run as plain use-package before the mode
;; is enabled, causing "Cannot load <pkg>" errors and a fatal boot.
(elpaca-wait)


;; ------------------------------------------------------------
;; Dash

(defvar core:dash t
  "If non-nil, show the coremacs dash on startup.")

(defun core:dash:default-buffer ()
  (let ((buffer (get-buffer-create " *coremacs*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n > coremacs\n\n"
                (propertize (format "   ready in %s" (emacs-init-time "%.2fs"))
                            'face 'shadow)
                "\n"))
      (setq-local cursor-type nil)
      (read-only-mode 1))
    buffer))

(defvar core:dash:make-buffer #'core:dash:default-buffer
  "Function that returns a buffer to display on startup.")

(defun core:dash-setup ()
  (setq initial-buffer-choice core:dash:make-buffer))

;; ------------------------------------------------------------
;; UI

(defvar core:font nil
  "Default font.  Setting it applies it to every frame.")

(defun core:ui:-font-changed (_symbol newval operation _where)
  (when (and newval (eq operation 'set) (display-graphic-p))
    (set-frame-font newval nil t)))

(defvar core:theme-faces-functions nil
  "Functions run after a theme is enabled, to (re)apply face tweaks.
Faces set with `set-face-attribute' are wiped out whenever a theme is
(re)loaded, so anything that styles a face based on the theme belongs
here rather than in a one-off `:config'.")

(defun core:-run-theme-faces-functions (&rest _)
  (run-hooks 'core:theme-faces-functions))

(defvar core:malloc-trim-idle-delay 60
  "Seconds of idle time before returning freed heap memory to the OS.
glibc tends to hang on to freed memory, so RSS keeps climbing even though
Lisp memory is flat.  nil disables it.")

(defun core:ui-setup ()
  (add-variable-watcher 'core:font #'core:ui:-font-changed)
  (add-hook 'enable-theme-functions #'core:-run-theme-faces-functions)
  (core:-run-theme-faces-functions)
  (when (and core:malloc-trim-idle-delay (fboundp 'malloc-trim))
    (run-with-idle-timer core:malloc-trim-idle-delay t #'malloc-trim)))

;; ------------------------------------------------------------
;; Mode line
;;
;; Everything in here is evaluated on *every* redisplay of *every*
;; window, so: no subprocesses, no keymap/face construction, and cache
;; anything that touches the filesystem.

(defvar core:mode-line-position 'top
  "Mode line position.  Either `top' (header line) or `bottom'.")

(defvar core:mode-line t
  "Set to non-nil to enable core:mode-line.")

(defvar core:mode-line-buffer-status-token-read-only " RO "
  "Token to show when buffer is read-only.")

(defvar core:mode-line-buffer-status-token-modified " ** "
  "Token to show when buffer is modified.")

(defvar core:mode-line-buffer-status-token-read-write " RW "
  "Token to show when buffer is writable.")

(defface core:mode-line-buffer-status-read-only '((t :foreground "red"))
  "Face for read-only buffer status.")

(defface core:mode-line-buffer-status-modified '((t :foreground "orange"))
  "Face for modified buffer status.")

(defface core:mode-line-buffer-status-read-write '((t :foreground "blue"))
  "Face for writable buffer status.")

(defface core:mode-line-project '((t :inherit mode-line))
  "Face for project display.")

(defface core:mode-line-flycheck-error '((t :inherit error))
  "Face for flycheck errors.")

(defface core:mode-line-flycheck-warning '((t :inherit warning))
  "Face for flycheck warnings.")

(defface core:mode-line-flycheck-info '((t :inherit success))
  "Face for flycheck info.")

(defface core:mode-line-major-mode '((t :inherit mode-line))
  "Face for mode line major mode.")

(defvar-local core:mode-line-buffer-status
    '(:eval (cond
             (buffer-read-only
              (propertize core:mode-line-buffer-status-token-read-only
                          'face 'core:mode-line-buffer-status-read-only))
             ((buffer-modified-p)
              (propertize core:mode-line-buffer-status-token-modified
                          'face 'core:mode-line-buffer-status-modified))
             (t
              (propertize core:mode-line-buffer-status-token-read-write
                          'face 'core:mode-line-buffer-status-read-write))))
  "Displays the current buffer status (read-only/modified/writable).")

;;; Project / branch, cached per buffer

(defvar core:project-info-ttl 5
  "Seconds to cache a buffer's project name and git branch.")

(defvar-local core:--project-info nil
  "Cache: (TIMESTAMP . (NAME . BRANCH)) or (TIMESTAMP . nil).")

(defun core:-compute-project-info ()
  (when-let* ((project (project-current)))
    (cons (project-name project)
          (let ((default-directory (project-root project)))
            (when (eq (ignore-errors (vc-responsible-backend default-directory)) 'Git)
              (require 'vc-git)
              (ignore-errors (car (vc-git-branches))))))))

(defun core:project-info ()
  "Return (NAME . BRANCH) for the current buffer's project, or nil.
Cached for `core:project-info-ttl' seconds, because `project-current'
walks the filesystem and finding the branch spawns git."
  (let ((now (float-time)))
    (unless (and core:--project-info
                 (< (- now (car core:--project-info)) core:project-info-ttl))
      (setq core:--project-info (cons now (core:-compute-project-info))))
    (cdr core:--project-info)))

(defun core:-invalidate-project-info ()
  (setq core:--project-info nil))

(add-hook 'after-save-hook #'core:-invalidate-project-info)
(add-hook 'find-file-hook #'core:-invalidate-project-info)

(defvar-local core:mode-line-project
    '(:eval (when-let* ((info (core:project-info)))
              (propertize (if (cdr info)
                              (format "(%s:%s)" (car info) (cdr info))
                            (format "(%s)" (car info)))
                          'face 'core:mode-line-project)))
  "Displays the active project and git branch for the current buffer.")

(defvar-local core:mode-line-major-mode
    '(:eval (propertize (capitalize (format-mode-line mode-name))
                        'face 'core:mode-line-major-mode)))

(defvar-local core:mode-line-flycheck
    '(:eval (when (bound-and-true-p flycheck-current-errors)
              (let ((counts (flycheck-count-errors flycheck-current-errors)))
                (mapconcat
                 (lambda (spec)
                   (let ((n (alist-get (car spec) counts)))
                     (when (and n (> n 0))
                       (propertize (format "%d " n) 'face (cdr spec)))))
                 '((error . core:mode-line-flycheck-error)
                   (warning . core:mode-line-flycheck-warning)
                   (info . core:mode-line-flycheck-info))))))
  "Displays flycheck error/warning/info counts for the current buffer.")

;;; Window controls (for undecorated frames)

(defvar core:minimize-frame-function #'iconify-frame)
(defvar core:maximize-frame-function #'toggle-frame-maximized)
(defvar core:close-frame-function #'save-buffers-kill-terminal)

(defun core:-click-map (fn-var)
  (let ((map (make-sparse-keymap))
        (cmd (lambda () (interactive) (call-interactively (symbol-value fn-var)))))
    (define-key map [header-line mouse-1] cmd)
    (define-key map [mode-line mouse-1] cmd)
    map))

(defvar core:mode-line-window-controls-minimize-map
  (core:-click-map 'core:minimize-frame-function))

(defvar core:mode-line-window-controls-maximize-map
  (core:-click-map 'core:maximize-frame-function))

(defvar core:mode-line-window-controls-close-map
  (core:-click-map 'core:close-frame-function))

(defface core:mode-line-window-controls-active '((t :inherit mode-line))
  "Face for visible window controls.")

(defface core:mode-line-window-controls-inactive
  '((t :inherit core:mode-line-window-controls-active))
  "Face for hidden window controls (same width, invisible colour).")

(defvar core:mode-line-window-controls-symbols-alist
  '((minimize . "—")
    (maximize . "▢")
    (close . "⤬")))

(defun core:-top-right-window-p (window)
  (and window
       (memq window (window-at-side-list nil 'top))
       (memq window (window-at-side-list nil 'right))
       t))

(defun core:-window-controls ()
  (let ((active (core:-top-right-window-p (get-buffer-window))))
    (mapcar (lambda (spec)
              (let ((symbol (alist-get (car spec) core:mode-line-window-controls-symbols-alist)))
                (if active
                    (propertize symbol
                                'local-map (symbol-value (cdr spec))
                                'mouse-face 'highlight
                                'face 'core:mode-line-window-controls-active)
                  (propertize symbol 'face 'core:mode-line-window-controls-inactive))))
            '((minimize . core:mode-line-window-controls-minimize-map)
              (maximize . core:mode-line-window-controls-maximize-map)
              (close . core:mode-line-window-controls-close-map)))))

(defvar-local core:mode-line-window-controls
    '(:eval (core:-window-controls)))

(defun core:-hide-inactive-window-controls ()
  "Paint inactive window controls in the bar's own background colour."
  (set-face-attribute 'core:mode-line-window-controls-inactive nil
                      :foreground (face-background
                                   (if (eq core:mode-line-position 'top) 'header-line 'mode-line)
                                   nil 'default)))

(add-hook 'core:theme-faces-functions #'core:-hide-inactive-window-controls)

;;; Right alignment

(defun core:-line-format-right-align (line-format)
  "`mode-line-format-right-align' tweak, allowing it to work in both the
header line and mode line."
  (let* ((rest (cdr (memq 'core:mode-line-format-right-align line-format)))
         (rest-str (format-mode-line `("" ,@rest)))
         (rest-width (progn
                       (add-face-text-property 0 (length rest-str) 'mode-line t rest-str)
                       (string-pixel-width rest-str))))
    (propertize " " 'display
                ;; The `right' spec doesn't work on TTY frames
                ;; when windows are split horizontally (bug#59620)
                (if (and (display-graphic-p)
                         (not (eq mode-line-right-align-edge 'window)))
                    `(space :align-to (- ,mode-line-right-align-edge (,rest-width)))
                  `(space :align-to (,(- (window-pixel-width)
                                         (window-scroll-bar-width)
                                         (window-right-divider-width)
                                         (* (or (car (window-margins)) 0)
                                            (frame-char-width))
                                         (pcase mode-line-right-align-edge
                                           ('right-margin (or (cdr (window-margins)) 0))
                                           ('right-fringe (or (cadr (window-fringes)) 0))
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

(defvar core:mode-line-format
  (list core:mode-line-buffer-status
        " "
        mode-line-buffer-identification
        " "
        core:mode-line-project
        'core:mode-line-format-right-align
        core:mode-line-flycheck
        core:mode-line-major-mode
        " "
        core:mode-line-window-controls))

(defun core:mode-line-setup ()
  (if (eq 'top core:mode-line-position)
      (setq-default mode-line-format nil
                    header-line-format core:mode-line-format)
    (setq-default header-line-format nil
                  mode-line-format core:mode-line-format)))

;; ------------------------------------------------------------

(defun core:init ()
  "Init coremacs."
  (when core:dash
    (core:dash-setup))
  (when core:mode-line
    (core:mode-line-setup))
  (core:ui-setup))

(provide 'coremacs)
