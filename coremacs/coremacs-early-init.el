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

;; LSP servers send big payloads; the 4k default means many tiny reads.
(setq read-process-output-max (* 1024 1024))

(provide 'coremacs-early-init)
