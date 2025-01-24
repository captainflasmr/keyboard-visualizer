;;; keyboard-visualizer.el --- Visualize keyboard shortcuts for commands -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Displays keyboard layouts in a buffer horizontally and highlights keys required
;; for interactive functions.  For multi-chord commands, displays
;; separate keyboard layouts side by side for each chord in the sequence.

;;; Code:

(require 'cl-lib)

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

(defcustom keyboard-visualizer-query-command-map
  '(;; File operations
    ("open file" . find-file)
    ("save file" . save-buffer)
    ("revert/restore file" . revert-buffer)
    ("rename file" . rename-buffer)

    ;; Editing
    ("undo" . undo)
    ("cut" . kill-region)
    ("copy" . kill-ring-save)
    ("paste" . yank)
    ("select all" . mark-whole-buffer)
    ("delete line" . kill-whole-line)
    ("delete word" . backward-kill-word)
    ("transpose words" . transpose-words)
    ("transpose lines" . transpose-lines)

    ;; Navigation
    ("search" . isearch-forward)
    ("search backward" . isearch-backward)
    ("replace" . query-replace)
    ("go to line" . goto-line)
    ("go to beginning of line" . move-beginning-of-line)
    ("go to end of line" . move-end-of-line)
    ("go to beginning of buffer" . beginning-of-buffer)
    ("go to end of buffer" . end-of-buffer)
    ("forward word" . forward-word)
    ("backward word" . backward-word)
    ("scroll up" . scroll-up-command)
    ("scroll down" . scroll-down-command)

    ;; Buffers
    ("next buffer" . next-buffer)
    ("previous buffer" . previous-buffer)
    ("switch buffer" . switch-to-buffer)
    ("save buffer" . save-buffer)

    ;; Windows
    ("split window horizontally" . split-window-right)
    ("split window vertically" . split-window-below)
    ("delete window" . delete-window)
    ("delete other windows" . delete-other-windows)
    ("switch window" . other-window)
    ("resize window" . enlarge-window)
    ("balance windows" . balance-windows)

    ;; Dired (File Explorer)
    ("open directory" . dired)

    ;; Frames
    ("new frame" . make-frame-command)
    ("delete frame" . delete-frame)
    ("next frame" . other-frame)

    ;; Macros
    ("start macro" . kmacro-start-macro)
    ("end macro" . kmacro-end-macro)
    ("play macro" . kmacro-end-and-call-macro)

    ;; Help
    ("describe shortcuts" . describe-key)
    ("list shortcuts" . describe-bindings)

    ;; Emacs Meta
    ("execute command" . execute-extended-command)
    ("quit emacs" . save-buffers-kill-terminal)
    )

  "Mapping of natural language queries to Emacs commands."
  :type '(alist :key-type string :value-type symbol)
  :group 'keyboard-visualizer)

(defface keyboard-visualizer-key-face
  '((t :inherit default :box (:line-width 1 :style released-button)))
  "Face for normal keys." :group 'keyboard-visualizer)

(defface keyboard-visualizer-highlight-face
  '((t :inherit highlight :box (:line-width 1 :style released-button)))
  "Face for highlighted keys." :group 'keyboard-visualizer)

(defface keyboard-visualizer-monospace-face
  '((t :family "Courier" :height 100))
  "Face for monospace text in the *Keyboard Layout* buffer."
  :group 'keyboard-visualizer)

(defvar keyboard-visualizer-insert-mode nil
  "Non-nil means `keyboard-visualizer-show-command` is called after every command.")

(defun keyboard-visualizer-save-layout ()
  "Append the current contents of the *Keyboard Layout* buffer to a file and visit the file in a buffer."
  (interactive)
  (let ((buffer (get-buffer "*Keyboard Layout*")))
    (if (not buffer)
        (message "No *Keyboard Layout* buffer to save.")
      (let ((file (read-file-name "Append keyboard layout to file: ")))
        (with-current-buffer buffer
          (write-region (point-min) (point-max) file 'append))
        (message "Appended *Keyboard Layout* to %s" file)
        (find-file file)))))

(defun keyboard-visualizer-learn-command-by-query ()
  "Prompt user for a command via query and visualize its keybinding."
  (interactive)
  (if-let* ((query (completing-read "Search for command: " (mapcar #'car keyboard-visualizer-query-command-map)))
            (command (cdr (assoc query keyboard-visualizer-query-command-map))))
      (keyboard-visualizer-show-command command)
    (message "Command not found.")))

(defun keyboard-visualizer--get-commands-with-keychords ()
  "Retrieve a list of interactive commands that have associated key chords."
  (let ((commands '()))
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (keyboard-visualizer--get-key-sequence symbol)) ;; Ensure it has a key chord
         (push symbol commands)))
     obarray)
    commands))

(defun keyboard-visualizer-learning-step ()
  "Manually step to the next interactive command in learning mode."
  (interactive)
  (let* ((commands (keyboard-visualizer--get-commands-with-keychords))
         (random-command (nth (random (length commands)) commands)))
    (keyboard-visualizer-show-command random-command)))

(defun keyboard-visualizer-learning-show-random-command ()
  "Show a random interactive command's keybinding."
  (interactive)
  (let* ((commands (keyboard-visualizer--get-commands-with-keychords))
         (random-command (nth (random (length commands)) commands)))
    (keyboard-visualizer-show-command random-command)))

(defun keyboard-visualizer--create-buffer ()
  "Create or get the keyboard visualization buffer and set it to use a monospace font."
  (let ((buffer (get-buffer-create "*Keyboard Layout*")))
    (with-current-buffer buffer
      (buffer-face-set 'keyboard-visualizer-monospace-face)
      (setq-local face-remapping-alist '((default keyboard-visualizer-monospace-face))))
    buffer))

(defun keyboard-visualizer--get-key-sequence (command)
  "Return key sequence for COMMAND or nil if none exists."
  (where-is-internal command nil t))

(defun keyboard-visualizer--break-chord (chord)
  "Break a key CHORD (e.g., \"C-x\") into individual keys."
  (mapcar (lambda (mod)
            (pcase mod ("C" "Ctrl") ("M" "Alt") ("S" "Shift") (_ mod)))
          (split-string chord "-")))

(defun keyboard-visualizer--key-sequence-to-chords (key-sequence)
  "Convert KEY-SEQUENCE to a list of chord lists."
  (mapcar #'keyboard-visualizer--break-chord
          (split-string
           (replace-regexp-in-string "[<>]" "" (key-description key-sequence)) " ")))

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
[j] Fuzzy Search for Command
[b] Keyboard Visualizer Toggle
[n] Step to Random Next Command
[s] Save Current Keyboard Layout"
               'face 'minibuffer-prompt))))
    (pcase key
      (?j (call-interactively 'keyboard-visualizer-learn-command-by-query))
      (?b (call-interactively 'keyboard-visualizer-insert-mode-toggle))
      (?n (call-interactively 'keyboard-visualizer-learning-step))
      (?s (call-interactively 'keyboard-visualizer-save-layout))
      (?q (message "Quit Shell menu."))
      (?\C-g (message "Quit Shell menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c b") 'keyvis-menu)
(global-set-key (kbd "M-c") 'keyboard-visualizer-learning-step)

(provide 'keyboard-visualizer)
;;; keyboard-visualizer.el ends here
