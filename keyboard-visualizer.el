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
  '(("ESC" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12")
    ("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "DEL")
    ("TAB" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
    ("Caps" "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "RET")
    ("Shift" "z" "x" "c" "v" "b" "n" "m" "," "." "/" "Shift")
    ("Ctrl" "Super" "Alt" " " " " "SPC" " " " " "RAlt" "RCtrl"))
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
  '((t :family "Courier" :height 160))
  "Face for monospace text in the *Keyboard Layout* buffer."
  :group 'keyboard-visualizer)

(defcustom keyboard-visualizer-learning-exclude-commands
  '(self-insert-command 
    keyboard-quit 
    keyboard-escape-quit 
    abort-recursive-edit
    execute-extended-command
    keyboard-visualizer-show-command
    keyboard-visualizer-learning-mode-toggle)
  "List of commands to exclude from the random function learning mode."
  :type '(repeat symbol)
  :group 'keyboard-visualizer)

(defvar keyboard-visualizer-insert-mode nil
  "Non-nil means `keyboard-visualizer-show-command` is called after every command.")

(defvar keyboard-visualizer-learning-mode nil
  "Non-nil means being able to step through random commands.")

(defvar keyboard-visualizer-current-command nil
  "Stores the current command being shown in learning mode.")

(defun keyboard-visualizer--get-commands-with-keychords ()
  "Retrieve a list of interactive commands that have associated key chords."
  (let ((commands '()))
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (not (memq symbol keyboard-visualizer-learning-exclude-commands))
                  (keyboard-visualizer--get-key-sequence symbol)) ;; Ensure it has a key chord
         (push symbol commands)))
     obarray)
    commands))

(defun keyboard-visualizer-learning-step ()
  "Manually step to the next interactive command in learning mode."
  (interactive)
  ;; (if keyboard-visualizer-learning-mode
      (let* ((commands (keyboard-visualizer--get-commands-with-keychords))
             (random-command (nth (random (length commands)) commands)))
        (setq keyboard-visualizer-current-command random-command)
        (keyboard-visualizer-show-command random-command)))
    ;; (message "Learning mode is not enabled. Enable it first with 'C-c b l'.")))

(defun keyboard-visualizer-learning-show-random-command ()
  "Show a random interactive command's keybinding."
  (interactive)
  (let* ((commands (keyboard-visualizer--get-commands-with-keychords))
         (random-command (nth (random (length commands)) commands)))
    (condition-case nil
        (progn
          (keyboard-visualizer-show-command random-command)))))
      ;; (error (message "Could not show keybinding for %s" random-command)))))

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
    (let* ((desc (replace-regexp-in-string "[<>]" "" (key-description key-sequence)))
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
           (all-layouts (mapcar #'keyboard-visualizer--draw-single-layout chord-lists))
           (num-rows (length (car all-layouts)))
           (spacing "    "))
      (erase-buffer)
      (goto-char (point-min))
      (insert (format "%s %s\n" (symbol-name command) (key-description sequences)))
      (dotimes (row-idx num-rows)
        (dolist (layout all-layouts)
          (insert (nth row-idx layout))
          (insert spacing))
        (insert "\n"))
        (goto-char (point-min)))))
        ;; (special-mode))))

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
            (with-current-buffer (keyboard-visualizer--create-buffer)
              (let* ((inhibit-read-only t)
                     ;; (description-length (window-total-width (get-buffer-window "*Keyboard Layout*")))
                     (description-length 1000)
                     (key-sequences (where-is-internal command nil t))
                     (description (or (replace-regexp-in-string "\n" " " (documentation command)) "No description available")))
                (goto-char (point-min))
                (insert (propertize "Description:" 'face 'bold) "\n"
                        (substring description 0 (min (length description) description-length))
                        (if (> (length description) description-length) "..." "") "\n")))
            (display-buffer (keyboard-visualizer--create-buffer)))))))
        ;; (message "No key binding found for %s" command)))))

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

;;;###autoload
(defun keyboard-visualizer-learning-mode-toggle ()
  "Toggle keyboard learning mode."
  (interactive)
  (setq keyboard-visualizer-learning-mode (not keyboard-visualizer-learning-mode))
  (if keyboard-visualizer-learning-mode
      (progn
        (remove-hook 'post-command-hook #'keyboard-visualizer--maybe-show-command)        
        (message "Keyboard learning mode enabled. Use 'C-c b n' to step manually."))
    (setq keyboard-visualizer-current-command nil)
    (message "Keyboard learning mode disabled.")))

(add-to-list 'display-buffer-alist
             '("\\*Keyboard Layout\\*"
               (display-buffer-in-side-window)
               (side . top)
               (slot . 0)
               (window-height . fit-window-to-buffer)
               ;; (window-height . 10)
               (preserve-size . (nil . t))
               (inhibit-same-window . t)))

(defun keyvis-menu ()
  "Menu for Shell commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "--- Keyboard Visualizer Commands [q] Quit: ---
[b] Keyboard Visualizer Toggle
[l] Keyboard Visualizer Learning
[n] Step to Next Command (Learning Mode Only)"
                'face 'minibuffer-prompt))))
    (pcase key
      (?b (call-interactively 'keyboard-visualizer-insert-mode-toggle))
      (?l (call-interactively 'keyboard-visualizer-learning-mode-toggle))
      (?n (call-interactively 'keyboard-visualizer-learning-step))
      (?q (message "Quit Shell menu."))
      (?\C-g (message "Quit Shell menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c b") 'keyvis-menu)
(global-set-key (kbd "M-c") 'keyboard-visualizer-learning-step)

(provide 'keyboard-visualizer)
;;; keyboard-visualizer.el ends here
