;;; keyboard-visualizer.el --- Visualize keyboard shortcuts for commands -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Displays keyboard layouts in a buffer horizontally and highlights keys required
;; for interactive functions.  For multi-chord commands, displays
;; separate keyboard layouts side by side for each chord in the sequence.

;;; Code:

(defgroup keyboard-visualizer nil
  "Visualize keyboard shortcuts on a keyboard layout."
  :group 'help)

(defcustom keyboard-visualizer-layout
  '(("ESC" "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12")
    ("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "DEL")
    ("TAB" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
    ("Caps" "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "Enter")
    ("Shift" "z" "x" "c" "v" "b" "n" "m" "," "." "/" "Shift")
    ("Ctrl" "W" "Alt" " " " " "SPC" " " " " "RAlt" "RCtrl"))
  "Keyboard layout representation."
  :type '(repeat (repeat string))
  :group 'keyboard-visualizer)

(defface keyboard-visualizer-key-face
  '((t :inherit default :box (:line-width 1 :style released-button)))
  "Face for normal keys."
  :group 'keyboard-visualizer)

(defface keyboard-visualizer-highlight-face
  '((t :inherit highlight :box (:line-width 1 :style released-button)))
  "Face for highlighted keys."
  :group 'keyboard-visualizer)

(defface keyboard-visualizer-monospace-face
  '((t :family "Courier"))
  "Face for monospace text in the *Keyboard Layout* buffer."
  :group 'keyboard-visualizer)

(defun keyboard-visualizer--create-buffer ()
  "Create or get the keyboard visualization buffer and set it to use a monospace font."
  (let ((buffer (get-buffer-create "*Keyboard Layout*")))
    (with-current-buffer buffer
      (buffer-face-set 'keyboard-visualizer-monospace-face)
      (setq-local face-remapping-alist '((default keyboard-visualizer-monospace-face))))
    buffer))

(defun keyboard-visualizer--get-key-sequence (command)
  "Get the key sequence for COMMAND."
  (where-is-internal command nil t))

(defun keyboard-visualizer--break-chord (chord)
  "Break a key CHORD (like \"C-x\") into individual keys (\"C\" \"x\").
Returns a list of keys that should be highlighted for this chord."
  (let* ((parts (split-string chord "-"))
         (last-key (car (last parts)))
         (modifiers (butlast parts)))
    (append
     ;; Convert modifier keys to their keyboard representation
     (mapcar (lambda (mod)
               (cond
                ((string= mod "C") "Ctrl")
                ((string= mod "M") "Alt")
                ((string= mod "S") "Shift")
                (t mod)))
             modifiers)
     (list last-key))))

(defun keyboard-visualizer--key-sequence-to-chords (key-sequence)
  "Convert KEY-SEQUENCE to a list of chord lists.
Each chord list contains the keys to be highlighted for that chord."
  (when key-sequence
    (let* ((desc (key-description key-sequence))
           (chords (split-string desc " ")))
      (mapcar #'keyboard-visualizer--break-chord chords))))

(defun keyboard-visualizer--draw-single-layout (chord-keys)
  "Draw a single keyboard layout with CHORD-KEYS highlighted.
Returns a list of strings, one for each row of the layout."
  (let ((layout-rows nil))
    (dolist (row keyboard-visualizer-layout)
      (let ((row-str ""))
        (dolist (key row)
          (let ((highlighted (cl-some (lambda (highlight)
                                      (string-match-p
                                       (concat "^" (regexp-quote highlight) "$")
                                       key))
                                    chord-keys)))
            (setq row-str
                  (concat row-str
                          (propertize (format "%2s " key)
                                    'face (if highlighted
                                            'keyboard-visualizer-highlight-face
                                          'keyboard-visualizer-key-face))))))
        (push row-str layout-rows)))
    (nreverse layout-rows)))

(defun keyboard-visualizer--draw-layouts (chord-lists command sequences)
  "Draw multiple keyboard layouts horizontally, one for each chord in CHORD-LISTS."
  (with-current-buffer (keyboard-visualizer--create-buffer)
    (let* ((inhibit-read-only t)
           ;; Generate all layouts first
           (all-layouts (mapcar #'keyboard-visualizer--draw-single-layout chord-lists))
           ;; Get the number of rows (should be same for all layouts)
           (num-rows (length (car all-layouts)))
           ;; Number of spaces between layouts
           (spacing "    "))
      
      (erase-buffer)
      (insert (format "%s %s\n\n" (symbol-name command) (key-description sequences)))
      
      ;; For each row in the layouts
      (dotimes (row-idx num-rows)
        ;; Insert each layout's corresponding row
        (dolist (layout all-layouts)
          (insert (nth row-idx layout))
          (insert spacing))
        (insert "\n")))
    
    (goto-char (point-min))
    (special-mode)))

;;;###autoload
(defun keyboard-visualizer-show-command (command)
  "Show the keyboard layouts with keys highlighted for each chord in COMMAND."
  (interactive "CCommand: ")
  (if (not (commandp command))
      (message "Invalid command: %s" command)
    (let* ((key-sequences (keyboard-visualizer--get-key-sequence command))
           (chord-lists (and key-sequences
                            (keyboard-visualizer--key-sequence-to-chords key-sequences))))
      (if chord-lists
          (progn
            (keyboard-visualizer--draw-layouts chord-lists command key-sequences)
            (display-buffer (keyboard-visualizer--create-buffer)))
        (message "No key binding found for %s" command)))))

(defvar keyboard-visualizer-insert-mode nil
  "Non-nil means `keyboard-visualizer-show-command` is called after every command.")

(defun keyboard-visualizer--maybe-show-command ()
  "Show the keyboard layout for the last executed command if insert mode is enabled."
  (when (and keyboard-visualizer-insert-mode
             (symbolp this-command)
             (commandp this-command))
    (keyboard-visualizer-show-command this-command)))

;;;###autoload
(defun keyboard-visualizer-insert-mode-toggle ()
  "Toggle keyboard visualizer insert mode."
  (interactive)
  (setq keyboard-visualizer-insert-mode (not keyboard-visualizer-insert-mode))
  (if keyboard-visualizer-insert-mode
      (progn
        (add-hook 'post-command-hook #'keyboard-visualizer--maybe-show-command)
        (message "Keyboard visualizer insert mode enabled."))
    (remove-hook 'post-command-hook #'keyboard-visualizer--maybe-show-command)
    (message "Keyboard visualizer insert mode disabled.")))

;; Configure how the keyboard layout buffer is displayed
(add-to-list 'display-buffer-alist
             '("\\*Keyboard Layout\\*"
               (display-buffer-in-side-window)
               (side . top)
               (slot . 0)
               (window-height . fit-window-to-buffer)
               (preserve-size . (nil . t))
               (inhibit-same-window . t)))

(provide 'keyboard-visualizer)
;;; keyboard-visualizer.el ends here
