;;; popup-term.el --- Bottom-of-frame terminal with tabs -*- lexical-binding: t; -*-

;; `popup-term-toggle' shows/hides an `eat' terminal in a bottom side
;; window.  Each terminal is a hidden buffer (" *terminal*",
;; " *terminal 1*", ...) shown as a tab in the window's tab line.
;;
;; Uses eat rather than term.el: term.el doesn't answer the capability
;; queries modern shells send (fish 4 prints them raw and then stalls
;; for 10s), has no truecolor and is slow on heavy output.

(require 'eat)
(require 'tab-line)

(defvar popup-term-tab-line-enabled t
  "Whether to show terminals as tabs in the tab line.")

(defvar popup-term-buffer-name "terminal"
  "Base name of popup terminal buffers.")

(defvar popup-term-shell (or (getenv "SHELL") shell-file-name)
  "Shell to run in a popup terminal.")

(defvar popup-term-window-height 0.3
  "Height of the popup window, as a fraction of the frame.")

(defvar popup-term-setup-hook nil
  "Hook run in a popup terminal buffer after it is set up.")

(defun popup-term--name (index)
  (if (zerop index)
      (format " *%s*" popup-term-buffer-name)
    (format " *%s %d*" popup-term-buffer-name index)))

(defun popup-term--index (buffer)
  "Return BUFFER's terminal index, or nil if it isn't a popup terminal."
  (let ((name (buffer-name buffer)))
    (cond
     ((null name) nil)                  ; killed buffer
     ((string= name (popup-term--name 0)) 0)
     ((string-match (format "\\` \\*%s \\([0-9]+\\)\\*\\'"
                            (regexp-quote popup-term-buffer-name))
                    name)
      (string-to-number (match-string 1 name))))))

(defun popup-term-is-term-buffer-p (&optional buffer)
  (and (popup-term--index (or buffer (current-buffer))) t))

(defun popup-term-buffer-list ()
  (sort (seq-filter #'popup-term-is-term-buffer-p (buffer-list))
        :key #'popup-term--index))

(defun popup-term-tab-name (buffer &optional _tabs)
  (format " %d \xf120 " (popup-term--index buffer)))

(defun popup-term-setup-tabs ()
  (when popup-term-tab-line-enabled
    (setq-local tab-line-tabs-function #'popup-term-buffer-list
                tab-line-new-button-functions '(popup-term-buffer-list)
                tab-line-new-tab-choice #'popup-term-new-tab
                tab-line-close-tab-function #'kill-buffer
                tab-line-tab-name-function #'popup-term-tab-name)
    (tab-line-mode 1)))

(defun popup-term--on-exit (process)
  "When a shell exits, show a sibling terminal or close the popup."
  ;; Deleting the selected window changes the current buffer, and eat's
  ;; own exit hook then kills *the current buffer* -- so keep it put.
  (save-current-buffer
    (when-let* ((buffer (process-buffer process))
                (window (get-buffer-window buffer t)))
      (if-let* ((other (seq-find (lambda (b) (not (eq b buffer)))
                                 (popup-term-buffer-list))))
          (set-window-buffer window other)
        (ignore-errors (delete-window window))))))

(defun popup-term-make-new-term ()
  "Create a new popup terminal buffer with the first free index."
  (let* ((index (seq-find (lambda (i) (not (get-buffer (popup-term--name i))))
                          (number-sequence 0 99)))
         (buffer (get-buffer-create (popup-term--name index))))
    (with-current-buffer buffer
      (eat-mode)
      (setq-local header-line-format nil
                  mode-line-format nil)
      (add-hook 'eat-exit-hook #'popup-term--on-exit nil t)
      (eat-exec buffer (substring (buffer-name) 2 -1) popup-term-shell nil nil)
      (popup-term-setup-tabs)
      (run-hooks 'popup-term-setup-hook))
    buffer))

(defun popup-term-new-tab ()
  "Open another terminal in the popup window."
  (interactive)
  (switch-to-buffer (popup-term-make-new-term)))

(defun popup-term-display-buffer (buffer)
  (let ((window (display-buffer-in-side-window
                 buffer
                 `((side . bottom)
                   (window-height . ,popup-term-window-height)
                   (window-parameters . ((no-delete-other-windows . t)
                                         (no-other-window . t)))))))
    (set-window-fringes window 0 0)
    window))

(defun popup-term ()
  "Display or switch to a popup terminal running `popup-term-shell'."
  (interactive)
  (let* ((buffer (or (car (popup-term-buffer-list))
                     (popup-term-make-new-term)))
         (window (or (seq-some #'get-buffer-window (popup-term-buffer-list))
                     (popup-term-display-buffer buffer))))
    (select-window window)))

(defun popup-term-hide ()
  "Hide the popup terminal, if it is visible."
  (interactive)
  (when-let* ((window (seq-some #'get-buffer-window (popup-term-buffer-list))))
    (delete-window window)))

;;;###autoload
(defun popup-term-toggle ()
  "Toggle the popup terminal."
  (interactive)
  (if (popup-term-is-term-buffer-p (window-buffer))
      (popup-term-hide)
    (popup-term)))

(provide 'popup-term)
;;; popup-term.el ends here
