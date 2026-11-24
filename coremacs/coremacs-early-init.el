;;; coremacs-early-init.el -*- lexical-binding: t; -*-

;; Kill the chrome before the first frame exists, instead of drawing it
;; and then removing it (which also triggers a frame resize).
(setq default-frame-alist
      (append '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil))
              default-frame-alist)
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq package-enable-at-startup nil     ; We use elpaca, not package.el
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      frame-title-format '(multiple-frames "%b" ("" "COREMACS [%b]")))

;; No GC while starting up; settle on something sane afterwards.  The
;; default (800k) collects constantly with LSP traffic, `most-positive-fixnum'
;; is a memory bomb.
(defvar core:gc-cons-threshold (* 32 1024 1024)
  "`gc-cons-threshold' to use once startup is done.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold core:gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; Every `load'/`require' during startup runs file names through the
;; handler regexps (TRAMP, compressed files...).  None apply to our own
;; files, so skip them until startup is done.
(defvar core:--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; keep anything added during startup, restore the rest
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       core:--file-name-handler-alist))))
          ;; early, before anything else on the hook touches files
          -90)

;; LSP servers send big payloads; the 4k default means many tiny reads.
(setq read-process-output-max (* 1024 1024))

(provide 'coremacs-early-init)
