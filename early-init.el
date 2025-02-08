(add-to-list 'load-path (expand-file-name "~/.emacs.d/coremacs"))

(dolist (dirname '("nano-tools" "nano-modeline"))
  (add-to-list 'load-path (expand-file-name dirname "~/.emacs.d/nano/")))

(require 'coremacs-early-init)
