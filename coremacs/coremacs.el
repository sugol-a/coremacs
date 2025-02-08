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
  (let ((buffer (get-buffer-create "*coremacs*")))
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
  (when core:font
    (core:ui:-set-font core:font))
  (add-variable-watcher 'core:font #'core:ui:-font-changed))

;; ------------------------------------------------------------

(defun core:init ()
  "Init coremacs"
  (when core:dash
    (core:dash-setup))
  (core:ui-setup))

(provide 'coremacs)
