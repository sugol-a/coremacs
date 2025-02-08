(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq package-enable-at-startup nil	      ; We don't use package.el
      gc-cons-threshold most-positive-fixnum ; Prevent gc during startup
      )

;; Cosmetics
(setq
 frame-title-format '(multiple-frames "%b" ("" "COREMACS [%b]"))
 inhibit-startup-screen t)

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 4 (default-value 'gc-cons-threshold)))))

(provide 'coremacs-early-init)
