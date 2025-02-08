(add-to-list 'load-path (expand-file-name "~/.emacs.d/coremacs"))

(dolist (dirname '("nano-tools" "nano-modeline"))
  (add-to-list 'load-path (expand-file-name dirname "~/.emacs.d/nano/")))

(add-to-list 'default-frame-alist
             '(drag-with-header-line . t))

(add-to-list 'default-frame-alist
             '(undecorated . t))

(add-to-list 'default-frame-alist '(internal-border-width . 6))

(require 'coremacs-early-init)
