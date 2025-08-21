(add-to-list 'load-path (expand-file-name "~/.emacs.d/coremacs"))

(add-to-list 'default-frame-alist
             '(drag-with-header-line . t))

(add-to-list 'default-frame-alist
             '(drag-internal-border . t))

(add-to-list 'default-frame-alist
             '(undecorated . t))

(add-to-list 'default-frame-alist '(internal-border-width . 6))

(add-to-list 'default-frame-alist '(font . "Adwaita Mono 13"))

(require 'coremacs-early-init)
