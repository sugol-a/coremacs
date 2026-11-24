;;; early-init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "coremacs" user-emacs-directory))

(setq default-frame-alist
      (append '((undecorated . t)
                (drag-with-header-line . t)
                (drag-internal-border . t)
                (internal-border-width . 6)
                (font . "Adwaita Mono 9"))
              default-frame-alist))

(require 'coremacs-early-init)
