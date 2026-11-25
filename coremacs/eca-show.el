;;; eca-show.el --- Let ECA agents show what they're working on -*- lexical-binding: t; -*-

;;; Commentary:
;; Called by agents via `emacsclient -e':
;;
;;   Show a file/region:
;;     emacsclient -s SERVER -e \
;;       '(eca-show-region "/abs/path" START END "NOTE" "AGENT-ID")'
;;
;;   Release when done:
;;     emacsclient -s SERVER -e '(eca-show-release "AGENT-ID")'
;;
;; SERVER defaults to "server"; AGENT-ID defaults to "main".
;; Each agent ID gets its own window on a visible graphical frame, a highlight
;; overlay on the lines it's working on, and a small child frame beside them
;; with a note.  Highlight and note colours come from the Catppuccin palette,
;; per agent.  `eca-show-clear' clears all agents at once.

;;; Code:

(require 'posframe)
(require 'seq)
(require 'color)

(defgroup eca-show nil
  "Show ECA agents' current focus."
  :group 'tools)

(defface eca-show-note
  '((t :inherit tooltip))
  "Face for the note child frame."
  :group 'eca-show)

(defcustom eca-show-accents '(lavender peach teal mauve sky green)
  "Catppuccin palette colours assigned to agents in order of first use."
  :type '(repeat symbol))

(defcustom eca-show-tint 0.2
  "How much of the accent to blend into the default background for highlights."
  :type 'number)

(defcustom eca-show-max-slots 4
  "Most agent windows to open; beyond this the least recently used is reused."
  :type 'integer)

(defcustom eca-show-note-width 60
  "Fill width of the note, in characters."
  :type 'integer)

(defvar eca-show--slots nil
  "Alist of (ID . plist) with :window :overlay :accent :created :used.")

(defun eca-show--slot (id) (alist-get id eca-show--slots nil nil #'equal))

(defun eca-show--slot-put (id prop value)
  (setf (alist-get id eca-show--slots nil nil #'equal)
        (plist-put (eca-show--slot id) prop value)))

(defun eca-show--note-buffer (id) (format " *eca-show-note:%s*" id))

;;;; Colours

(defun eca-show--accent-colour (accent)
  "Hex colour for ACCENT from the current Catppuccin flavour, or nil."
  (and (fboundp 'catppuccin-color)
       (memq 'catppuccin custom-enabled-themes)
       (ignore-errors (catppuccin-color accent))))

(defun eca-show--next-accent ()
  (let ((used (mapcar (lambda (s) (plist-get (cdr s) :accent)) eca-show--slots)))
    (or (seq-find (lambda (a) (not (memq a used))) eca-show-accents)
        (nth (mod (length eca-show--slots) (length eca-show-accents)) eca-show-accents))))

(defun eca-show--highlight-face (accent)
  "Face plist tinting ACCENT into the default background."
  (let ((hex (eca-show--accent-colour accent))
        (base (face-attribute 'default :background nil t)))
    (if (and hex (color-defined-p base))
        `(:background ,(apply #'color-rgb-to-hex
                              (append (color-blend (color-name-to-rgb hex)
                                                   (color-name-to-rgb base)
                                                   eca-show-tint)
                                      '(2)))
          :extend t)
      '(:inherit secondary-selection :extend t))))

;;;; Windows

(defun eca-show--frame-windows ()
  "Windows on visible graphical top-level frames (skips the daemon's F1)."
  (seq-mapcat (lambda (f) (window-list f 'nomini))
              (seq-filter (lambda (f) (and (frame-visible-p f)
                                           (display-graphic-p f)
                                           (not (frame-parent f))))
                          (frame-list))))

(defun eca-show--usable-p (w)
  (not (or (window-parameter w 'window-side)
           (string-match-p "eca" (buffer-name (window-buffer w))))))

(defun eca-show--owner (w)
  (car (seq-find (lambda (s) (eq (plist-get (cdr s) :window) w)) eca-show--slots)))

(defun eca-show--window-area (w) (* (window-total-width w) (window-total-height w)))

(defun eca-show--split (w)
  (if (>= (window-total-width w) 160)
      (split-window w nil 'right)
    (split-window w nil 'below)))

(defun eca-show--window-for (id)
  "ID's window: its previous one, a free one, a new split, or the LRU slot."
  (let* ((own (plist-get (eca-show--slot id) :window))
         (wins (seq-filter #'eca-show--usable-p (eca-show--frame-windows)))
         (live-slots (seq-filter (lambda (s) (window-live-p (plist-get (cdr s) :window)))
                                 eca-show--slots)))
    (cond
     ((window-live-p own) own)
     ((seq-find (lambda (w) (not (eca-show--owner w))) wins))
     ((and wins (< (length live-slots) eca-show-max-slots))
      (let ((w (eca-show--split (car (sort (copy-sequence wins)
                                           (lambda (a b) (> (eca-show--window-area a)
                                                            (eca-show--window-area b))))))))
        (eca-show--slot-put id :created t)
        w))
     (live-slots
      (let ((lru (car (sort (copy-sequence live-slots)
                            (lambda (a b) (time-less-p (plist-get (cdr a) :used)
                                                       (plist-get (cdr b) :used)))))))
        (eca-show--clear-slot (car lru))
        (plist-get (cdr lru) :window)))
     ((seq-find (lambda (w) (not (window-parameter w 'window-side))) (eca-show--frame-windows)))
     (t (error "eca-show: no visible graphical frame")))))

;;;; Notes

(defun eca-show--poshandler (info)
  "Place the note at the right edge of the window, level with the highlight."
  (let ((point-pos (posframe-poshandler-point-top-left-corner info)))
    (cons (- (+ (plist-get info :parent-window-left)
                (plist-get info :parent-window-width))
             (plist-get info :posframe-width)
             16)
          (cdr point-pos))))

(defun eca-show--note-string (id accent note)
  (with-temp-buffer
    (insert note)
    (let ((fill-column eca-show-note-width))
      (fill-region (point-min) (point-max)))
    (concat (propertize (format "ECA · %s" id)
                        'face `(:weight bold :foreground ,(or (eca-show--accent-colour accent)
                                                              (face-attribute 'font-lock-keyword-face :foreground nil t))))
            "\n" (buffer-string))))

(defun eca-show--hidehandler (win buf)
  "Hide only once WIN stops showing BUF, not when you work in another buffer."
  (lambda (_info) (not (and (window-live-p win) (eq (window-buffer win) buf)))))

;;;; Commands

(defun eca-show--clear-slot (id)
  (let ((ov (plist-get (eca-show--slot id) :overlay)))
    (when (overlayp ov) (delete-overlay ov)))
  (posframe-hide (eca-show--note-buffer id)))

;;;###autoload
(defun eca-show-release (id)
  "Free agent ID's slot, closing its window if eca-show created it."
  (interactive (list (completing-read "Release agent: " (mapcar #'car eca-show--slots))))
  (let* ((slot (eca-show--slot id))
         (w (plist-get slot :window)))
    (eca-show--clear-slot id)
    (posframe-delete (eca-show--note-buffer id))
    (when (and (plist-get slot :created) (window-live-p w) (window-deletable-p w))
      (delete-window w))
    (setq eca-show--slots (assoc-delete-all id eca-show--slots))))

;;;###autoload
(defun eca-show-clear ()
  "Remove every agent's highlight and note (windows are kept)."
  (interactive)
  (mapc (lambda (s) (eca-show--clear-slot (car s))) eca-show--slots))

;;;###autoload
(defun eca-show-region (file &optional start end note id)
  "Show FILE for agent ID, highlight lines START to END, and NOTE beside them.
FILE is expanded relative to `default-directory' if not absolute.
ID defaults to \"main\". Return the buffer name."
  (let* ((file (expand-file-name file))
         (id (if (and id (not (string-empty-p id))) id "main"))
         (accent (or (plist-get (eca-show--slot id) :accent) (eca-show--next-accent))))
    (eca-show--clear-slot id)
    (let ((win (eca-show--window-for id)))
      (eca-show--slot-put id :accent accent)
      (eca-show--slot-put id :window win)
      (eca-show--slot-put id :used (current-time))
      (with-selected-window win
        (find-file file)
        (auto-revert-mode 1)
        (if (not (and start (> start 0)))
            (goto-char (point-min))
          (let* ((beg (progn (goto-char (point-min)) (forward-line (1- start)) (point)))
                 (end (progn (goto-char (point-min)) (forward-line (1- (or end start))) (line-end-position)))
                 (ov (make-overlay beg end)))
            (overlay-put ov 'face (eca-show--highlight-face accent))
            (overlay-put ov 'priority 100)
            (overlay-put ov 'window win)
            (eca-show--slot-put id :overlay ov)
            (goto-char beg)
            (recenter 5)
            (when (and note (not (string-empty-p note)) (posframe-workable-p))
              (redisplay)
              (posframe-show (eca-show--note-buffer id)
                             :string (eca-show--note-string id accent note)
                             :position beg
                             :poshandler #'eca-show--poshandler
                             :face 'eca-show-note
                             :internal-border-width 8
                             :internal-border-color (or (eca-show--accent-colour accent)
                                                        (face-attribute 'region :background nil t))
                             :hidehandler (eca-show--hidehandler win (current-buffer))))))
        (buffer-name)))))

(provide 'eca-show)
;;; eca-show.el ends here
