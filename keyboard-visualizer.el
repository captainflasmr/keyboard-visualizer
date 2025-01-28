;;; keyboard-visualizer.el --- Visualize keyboard shortcuts -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Displays keyboard layouts in a buffer horizontally and highlights keys required
;; for interactive functions.  For multi-chord commands, displays
;; separate keyboard layouts side by side for each chord in the sequence.

;;; Code:

(require 'cl-lib)

(defgroup keyboard-visualizer nil "Visualize keyboard shortcuts." :group 'help)

(defface keyboard-visualizer-non-ergonomic-face '((t :inherit bold :background "#ff0000" :foreground "#ffffff"))
  "Face for non-ergonomic combinations." :group 'keyboard-visualizer)

(defface keyboard-visualizer-key-face '((t :inherit default :box (:line-width 1 :style released-button)))
  "Normal key face." :group 'keyboard-visualizer)

(defface keyboard-visualizer-highlight-face '((t :inherit highlight :box (:line-width 1 :style released-button)))
  "Highlighted key face." :group 'keyboard-visualizer)

(defface keyboard-visualizer-monospace-face '((t :family "Courier" :height 100))
  "Monospace face." :group 'keyboard-visualizer)

(defcustom keyboard-visualizer-layout
  '(("ESC" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12")
    ("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "DEL")
    ("TAB" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
    ("Caps" "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "RET")
    ("Shift" "z" "x" "c" "v" "b" "n" "m" "," "." "/" "Shift")
    ("Ctrl" "Super" "Alt" " " " " "SPC" " " " " "RAlt" "RCtrl"))
  "Keyboard layout." :type '(repeat (repeat string)) :group 'keyboard-visualizer)

(defvar keyboard-visualizer--shift-map
  '(("!" . "1") ("@" . "2") ("#" . "3") ("$" . "4") ("%" . "5")
    ("^" . "6") ("&" . "7") ("*" . "8") ("(" . "9") (")" . "0")
    ("_" . "-") ("+" . "=") ("{" . "[") ("}" . "]") ("|" . "\\")
    (":" . ";") ("\"" . "'") ("<" . ",") (">" . ".") ("?" . "/")
    ("~" . "`"))
  "Mapping of shifted characters to their base keys.")

(defun keyboard-visualizer--generate-key-sides ()
  "Generate key sides based on keyboard-visualizer-layout.
Each row is split in half, with keys in the left half marked as 'left
and keys in the right half marked as 'right."
  (let (key-sides)
    (dolist (row keyboard-visualizer-layout)
      (let* ((row-length (length row))
             (middle (/ row-length 2))
             (left-keys (cl-subseq row 0 middle))
             (right-keys (cl-subseq row middle)))
        ;; Add left side keys
        (dolist (key left-keys)
          (push (cons key 'left) key-sides))
        ;; Add right side keys
        (dolist (key right-keys)
          (push (cons key 'right) key-sides))))
    key-sides))

(defvar keyboard-visualizer-insert-mode nil)
(defvar keyboard-visualizer-ergonomic-indicator nil)
(defvar keyboard-visualizer-show-documentation nil
  "When non-nil, show command documentation in the keyboard layout buffer.")

(defcustom keyboard-visualizer-query-command-map
  '(
    ;; File operations
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
    ("quit emacs" . save-buffers-kill-terminal))
  "Mapping of natural language queries to Emacs commands."
  :type '(alist :key-type string :value-type symbol)
  :group 'keyboard-visualizer)

(defcustom keyboard-visualizer-key-remaps
  '(("Caps" . "Ctrl")
    ("RAlt" . "Ctrl"))
  "Alist of key remappings. Each entry is a cons of (original-key . remapped-key)."
  :type '(alist :key-type string :value-type string)
  :group 'keyboard-visualizer)

(defun keyboard-visualizer-remap-key (key)
  "Return remapped key if it exists in `keyboard-visualizer-key-remaps', otherwise return key."
  (or (cdr (assoc key keyboard-visualizer-key-remaps)) key))

(defun keyboard-visualizer-add-remap ()
  "Add a new key remapping."
  (interactive)
  (let* ((orig-key (completing-read "Original key: " 
                                    (apply #'append keyboard-visualizer-layout)))
         (new-key (completing-read "Remap to: " 
                                   (delete-dups 
                                    (apply #'append keyboard-visualizer-layout)))))
    (setq keyboard-visualizer-key-remaps
          (cons (cons orig-key new-key)
                (assoc-delete-all orig-key keyboard-visualizer-key-remaps)))
    (message "Remapped %s to %s" orig-key new-key)))

(defun keyboard-visualizer-remove-remap ()
  "Remove an existing key remapping."
  (interactive)
  (let* ((remap (completing-read "Remove remap: " 
                                 (mapcar (lambda (remap)
                                           (format "%s -> %s" 
                                                   (car remap) 
                                                   (cdr remap)))
                                         keyboard-visualizer-key-remaps)))
         (key (car (split-string remap " -> "))))
    (setq keyboard-visualizer-key-remaps
          (assoc-delete-all key keyboard-visualizer-key-remaps))
    (message "Removed remapping for %s" key)))

(defun keyboard-visualizer-clear-remaps ()
  "Clear all key remappings."
  (interactive)
  (setq keyboard-visualizer-key-remaps nil)
  (message "Cleared all key remappings"))

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

(defun keyboard-visualizer--create-buffer ()
  (let ((buf (get-buffer-create "*Keyboard Layout*")))
    (with-current-buffer buf
      (buffer-face-set 'keyboard-visualizer-monospace-face)
      (setq-local face-remapping-alist '((default keyboard-visualizer-monospace-face))))
    buf))

(defun keyboard-visualizer--break-chord (chord)
  "Break a chord into its component keys, handling shifted characters."
  (let* ((parts (split-string chord "-"))
         (last-key (car (last parts)))
         (modifiers (butlast parts))
         (shifted-key (assoc last-key keyboard-visualizer--shift-map))
         (final-keys
          (append
           (mapcar (lambda (mod)
                    (pcase mod
                      ("C" "Ctrl")
                      ("M" "Alt")
                      ("S" "Shift")
                      (_ mod)))
                  modifiers)
           (when shifted-key
             (list "Shift" (cdr shifted-key)))
           (unless shifted-key
             (list last-key)))))
    final-keys))

(defun keyboard-visualizer--key-sequence-to-chords (key-sequence)
  (mapcar #'keyboard-visualizer--break-chord
          (split-string (replace-regexp-in-string "[<>]" "" (key-description key-sequence)) " ")))

(defun keyboard-visualizer--draw-single-layout (chord-keys &optional highlight-non-ergonomic)
  (let* ((key-sides (keyboard-visualizer--generate-key-sides))
         (remapped-chord-keys (mapcar #'keyboard-visualizer-remap-key chord-keys))
         (chord-sides (mapcar (lambda (k) 
                                (cdr (assoc k key-sides)))
                              remapped-chord-keys))
         (layout-rows
          (mapcar
           (lambda (row)
             (mapconcat
              (lambda (key)
                (let* ((remapped-key (keyboard-visualizer-remap-key key))
                       (highlighted (member remapped-key remapped-chord-keys))
                       (non-ergonomic (and highlight-non-ergonomic 
                                           highlighted
                                           (> (length chord-sides) 1)
                                           (apply #'eq chord-sides))))
                  (propertize (format "%2s " key)
                              'face (cond (non-ergonomic 
                                           'keyboard-visualizer-non-ergonomic-face)
                                          (highlighted 
                                           'keyboard-visualizer-highlight-face)
                                          (t 'keyboard-visualizer-key-face))
                              'help-echo (if (string= key remapped-key)
                                             nil
                                           (format "Remapped to %s" remapped-key)))))
              row ""))
           keyboard-visualizer-layout)))
    layout-rows))

(defun keyboard-visualizer-show-command (command)
  "Show keyboard visualization for COMMAND, with special handling for lambda functions."
  (interactive "CCommand: ")
  (when (or (commandp command) (functionp command))
    (let ((key-sequences (where-is-internal command nil t))
          (is-lambda (and (symbolp command)
                         (let ((func (symbol-function command)))
                           (and (consp func)
                                (memq (car func) '(lambda closure)))))))
      (with-current-buffer (keyboard-visualizer--create-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Display command name and type
          (insert (format "%s%s\n"
                         (if is-lambda
                             (prin1-to-string (symbol-function command))
                           command)
                         (if key-sequences
                             (concat " " (key-description key-sequences))
                           " (No key binding)")))
          
          ;; Draw keyboard layouts for key sequences if they exist
          (when key-sequences
            (let* ((chord-lists (keyboard-visualizer--key-sequence-to-chords key-sequences))
                   (all-layouts (mapcar
                                (lambda (chord)
                                  (keyboard-visualizer--draw-single-layout 
                                   chord keyboard-visualizer-ergonomic-indicator))
                                chord-lists)))
              (dotimes (row-idx (length (car all-layouts)))
                (dolist (layout all-layouts)
                  (insert (nth row-idx layout) "    "))
                (insert "\n"))))
          
          ;; Show documentation and/or lambda definition
          (when keyboard-visualizer-show-documentation
            (insert "\nDescription:\n")
            (if (documentation command)
                (insert (documentation command))
              (if is-lambda
                  (insert (format "Lambda function:\n%s"
                                (pp-to-string (symbol-function command))))
                (insert "No description available")))))
        (display-buffer (current-buffer))))))

(defun keyboard-visualizer--maybe-show-command ()
  (when (and keyboard-visualizer-insert-mode (commandp this-command))
    (keyboard-visualizer-show-command this-command)))

(defun keyboard-visualizer-documentation-toggle ()
  "Toggle display of command documentation."
  (interactive)
  (setq keyboard-visualizer-show-documentation (not keyboard-visualizer-show-documentation))
  (message "Command documentation display %s"
           (if keyboard-visualizer-show-documentation "enabled" "disabled")))

(defun keyboard-visualizer-insert-mode-toggle ()
  (interactive)
  (setq keyboard-visualizer-insert-mode (not keyboard-visualizer-insert-mode))
  (if keyboard-visualizer-insert-mode
      (add-hook 'post-command-hook #'keyboard-visualizer--maybe-show-command)
    (progn
      (remove-hook 'post-command-hook #'keyboard-visualizer--maybe-show-command)
      (when-let ((window (get-buffer-window "*Keyboard Layout*")))
        (delete-window window))
      (when-let ((buffer (get-buffer "*Keyboard Layout*")))
        (kill-buffer buffer))))
  (message "Keyboard visualizer insert mode %s." (if keyboard-visualizer-insert-mode "enabled" "disabled")))

(defun keyboard-visualizer-learn-command-by-query ()
  (interactive)
  (if-let* ((query (completing-read "Search for command: " (mapcar #'car keyboard-visualizer-query-command-map)))
            (command (cdr (assoc query keyboard-visualizer-query-command-map))))
      (keyboard-visualizer-show-command command)
    (message "Command not found.")))

(defun keyboard-visualizer-learning-step ()
  (interactive)
  (let* ((commands (cl-remove-if-not (lambda (sym) (and (commandp sym) (where-is-internal sym nil t)))
                                     (mapcar #'intern (all-completions "" obarray))))
         (command (nth (random (length commands)) commands)))
    (keyboard-visualizer-show-command command)))

(add-to-list 'display-buffer-alist
             '("\\*Keyboard Layout\\*"
               (display-buffer-in-side-window)
               (side . top)
               (slot . 0)
               (window-height . fit-window-to-buffer)
               (preserve-size . (nil . t))
               (inhibit-same-window . t)))

(defun keyboard-visualizer-menu ()
  "Menu for Shell commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "--------- Keyboard Visualizer Commands [q] Quit: -------
Toggle -> [b] Visualizer [d] Documentation [e] Ergonomic
Explore-> [f] Simple     [n] Random
Remap  -> [r] Add        [x] Remove         [c] Clear
Save   -> [s] Current"
               'face 'minibuffer-prompt))))
    (pcase key
      (?e (setq keyboard-visualizer-ergonomic-indicator 
                (not keyboard-visualizer-ergonomic-indicator)))
      (?f (call-interactively 'keyboard-visualizer-learn-command-by-query))
      (?b (call-interactively 'keyboard-visualizer-insert-mode-toggle))
      (?n (call-interactively 'keyboard-visualizer-learning-step))
      (?d (keyboard-visualizer-documentation-toggle))
      (?s (call-interactively 'keyboard-visualizer-save-layout))
      (?r (call-interactively 'keyboard-visualizer-add-remap))
      (?x (call-interactively 'keyboard-visualizer-remove-remap))
      (?c (call-interactively 'keyboard-visualizer-clear-remaps))
      (?q (message "Quit Shell menu."))
      (?\C-g (message "Quit Shell menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c b") 'keyboard-visualizer-menu)

(provide 'keyboard-visualizer)
