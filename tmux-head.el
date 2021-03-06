;; Copyright (c) 2019 Jeff Yuanqian Li

;; Author: Jeff Yuanqian Li

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'comint)

(setq comint-input-ring-size 2000)
(setq comint-input-ring-file-name "~/.shell_history")

(add-hook 'kill-emacs-hook #'comint-write-input-ring)

(defun tmux-run-command (cmd &rest args)
  (let ((retval (apply 'call-process "tmux" nil nil nil cmd args)))
    (unless (zerop retval)
      (error (format "Failed: %s (status = %d)"
                     (mapconcat 'identity (cons "tmux" args) " ")
                     retval)))))

(defun tmux-pane-0-run-command (cmd &rest args)
  (apply 'tmux-run-command cmd "-t" "0" args))

(defun tmux-send-key (&rest args)
  (apply 'tmux-pane-0-run-command "send-keys" args))

(defun tmux-run-key (&rest args)
  (apply 'tmux-pane-0-run-command "send-keys" (nconc args '("C-m"))))

(defun tmux-copy (str)
  (funcall 'tmux-run-command "set-buffer" "-b" "abuf" str))

(defun tmux-paste ()
  (funcall 'tmux-pane-0-run-command "paste-buffer" "-p" "-b" "abuf")
  (tmux-send-key "C-m"))

(defun tmux-shell-send-string (str &optional arg)
    (if arg
        (setq str (read-string "$ " str)))
    (setq str (replace-regexp-in-string ";\\'" "\\\\;" str))
    (tmux-copy str)
    (tmux-paste)
    (tmux-send-key "C-m"))

(defun tmux-send-region (&optional arg)
  (interactive "P")
  (let* (beg end cmd-str no-history)
    (cond
     ((eq major-mode 'python-mode)
      (unless (equal "python" (substring (tmux-pane-0-login-type) 0 6))
        (error "must run in a ipthon shell"))))
    (cond
     (mark-active
      (setq beg (region-beginning) 
            end (region-end))
      (deactivate-mark t)
      (unless (= (line-number-at-pos beg) (line-number-at-pos end))
        (setq no-history t)))
     ((and (eq major-mode 'org-mode) (org-in-item-p))
      (save-excursion
        (beginning-of-line)
        (looking-at org-list-full-item-re)
        (setq pos (car (last (match-data))))
        (goto-char pos)
        (if (looking-at "[ \t]*\\$[ \t]+")
            (goto-char (match-end 0)))
        (setq beg (point)
              end (line-end-position))))
     (t
      (save-excursion
        (setq beg (line-beginning-position)
              end (line-end-position)))))
    (tmux-shell-send-string (buffer-substring-no-properties beg end))
    (unless comint-input-ring
      (comint-read-input-ring 'silent))
    (unless no-history
      (comint-add-to-input-history cmd-str))
    (when (eq major-mode 'python-mode)
      (forward-line)
      (python-util-forward-comment 1))
    ))

(defun tmux-dired-run-file ()
  (interactive)
  (let ((cmd-str (dired-get-filename)))
    (tmux-run-key cmd-str)
    ))

(defun tmux-send-selection (cmd-str)
  (tmux-run-key cmd-str)
  (comint-add-to-input-history cmd-str)
  )

(defun tmux-minibuffer-run-shell-cmd ()
  (interactive)
  (let ((map minibuffer-local-map)
        cmd-str)
    (define-key map (kbd "TAB") 'my-complete-file-name)
    (define-key map (kbd "M-y") 'insert-current-file-name-at-point)
    (setq cmd-str (read-string "Shell Cmd: "))
    (tmux-send-selection cmd-str)))

(defun tmux-edit-and-send (cmd-string)
  (let ((cmd (read-string "$ " cmd-string)))
    cmd))

(defun tmux-edit-and-send-action ()
  (interactive)
  (ivy-exit-with-action
   (lambda (_)
     (funcall 'tmux-edit-and-send (ivy-state-current ivy-last)))))

(defun tmux-edit-and-send-prompt-action ()
  (interactive)
  (setf (ivy-state-current ivy-last) ivy-text)
  (tmux-edit-and-send-action)
  )

(fset 'my-complete-file-name
      (make-hippie-expand-function '(try-complete-file-name-partially
                                     try-complete-file-name)))

(defvar ivy-minibuffer-tmux-shell-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'my-complete-file-name)
    (define-key map (kbd "C-c C-c") #'tmux-ctrl-c)  
    (define-key map (kbd "C-d") #'tmux-ctrl-d)  
    (define-key map (kbd "M-RET") 'tmux-edit-and-send-prompt-action)
    (define-key map (kbd "M-h") 'tmux-swap-pane)
    (define-key map (kbd "M-i") #'ivy-immediate-done)   ;; CMD-m : run prompt text
    (define-key map (kbd "M-l") 'ivy-insert-current)    ;; CMD-h : insert selected
    (define-key map (kbd "M-m") 'tmux-edit-and-send-action)
    (define-key map (kbd "M-u") 'tmux-sync-location-with-emacs)
    map)
  "Keymap used for completing tmux shell commands in minibuffer.")

(defun tmux-ivy-run-shell (&optional arg)
  (interactive "P")
  (unless (file-exists-p comint-input-ring-file-name)
    (write-region "" nil comint-input-ring-file-name))
  (unless comint-input-ring
    (comint-read-input-ring 'silent))
  (if (equal arg '(4))
      (counsel--browse-history comint-input-ring)
    (catch :exit
      (ivy-read "Command to run: "
                (delete-dups
                 (when (> (ring-size comint-input-ring) 0)
                   (ring-elements comint-input-ring)))
                :keymap ivy-minibuffer-tmux-shell-command-map
                :action #'tmux-send-selection))))

(defun tmux-insert-state ()
  (interactive)
  (let (done c)
    (message "======tmux=======tmux======tmux======")
    (catch :exit
      (while t
        (setq c (read-key))
        (if (and (characterp c) (or (= c 13) (= c 27)))
            (if (eq done t)
                (progn
                  (message "done")
                  (throw :exit nil))
              (setq done t)
              (message "======tmux=======tmux======tmux======")
              (if (= c 13)
                  (tmux-send-key "C-m")
                (tmux-send-key "C-c")))
          (setq done nil)
          (cond
           ((eq c 'up)
            (tmux-send-key "Up"))
           ((eq c 'down)
            (tmux-send-key "Down"))
           ((eq c 'left)
            (tmux-send-key "Left"))
           ((eq c 'right)
            (tmux-send-key "Right"))
           ((= c 20) ;; ctrl-t
            (tmux-send-key "C-t")
            (setq done nil))
           ((= c 25) ;; ctrl-y
            (tmux-send-key (current-kill 0 t))
            )
           (t
            (message "======tmux=======tmux======tmux======")
            (tmux-send-key (char-to-string c))
            )))))))

(defun tmux-set-emacs-frame-name (frame-name)
  (let ((window-id (tmux-window-id)))
    (set-frame-parameter (car (frame-list)) 'name (concat "tmux-" window-id))
    ))

(add-hook 'after-make-frame-functions #'tmux-set-emacs-frame-name nil)

(defun tmux-display-pane-numbers ()
  (interactive)
  (message "wait ...")
  (tmux-run-command "display-panes" "-d" "500")
  (sit-for 0.51)
  (tmux-pane-0-run-command "select-pane")
  (message "")
  )

(defun my/tmux-select-number (num-list prompt)
  (let (char-list selected-number)
    (if (= (length num-list) 0)
        (error "empty selection list"))
    (if (= (length num-list) 1)
        (car num-list)
      (setq char-list (mapcar (lambda (x) (string-to-char x)) num-list)
            prompt (concat prompt
                           (mapconcat (lambda (x) (concat "[" x "]"))
                                      num-list " "))
            num (char-to-string (read-char-choice prompt char-list)))
      (message "")
      num)))

(defun tmux-get-list-entry (num &rest cmd)
  (with-temp-buffer
    (apply 'call-process "tmux" nil t nil cmd)
    (goto-char (point-min))
    (catch :exit
      (while (re-search-forward "^\\([0-9]\\):.*" nil t)
        (if (equal num (match-string-no-properties 1))
            (throw :exit (match-string-no-properties 0)))))))

(defun tmux-get-window-name (window-num)
  (let ((entry (tmux-get-list-entry window-num "list-windows")))
    (if entry
        (if (string-match "^[0-9]:\\s-+\\(\\sw+\\)" entry)
            (match-string-no-properties 1 entry)))))

(defun tmux-get-pane-or-window-number-list (&optional panes)
  (let ((list-cmd (if panes "list-panes" "list-windows"))
        list)
    (with-temp-buffer
      (funcall 'call-process "tmux" nil t nil list-cmd)
      (setq list
            (mapcar
             (lambda (str)
               (let ((regex
                      "^\\([0-9]\\):"))
                 (when (string-match regex str)
                   (match-string 1 str))))
             (split-string (buffer-string) "\n" t))))
    list))

(defun tmux-display-message (message &optional pane-id)
  (with-temp-buffer
    (funcall 'call-process "tmux" nil t nil "display" "-t" (or pane-id "0") "-p" message)
    (goto-char (point-min))
    (replace-regexp-in-string "'" "" (buffer-substring-no-properties (point) (point-at-eol)))))

(defun tmux-pane-height (pane-id)
  (tmux-display-message "#{pane_height}" pane-id))

(defun tmux-window-height ()
  (tmux-display-message "#{window_height}"))

(defun tmux-window-id ()
  (tmux-display-message "#I"))

(defun tmux-zoomed ()
  (let ((status (tmux-display-message "#{window_zoomed_flag}")))
    (equal "1" status)))

(defun tmux-zoom-pane-0 ()
  (unless (tmux-zoomed)
    (tmux-toggle-zoom))
  )

(defun tmux-unzoom-pane-0 ()
  (if (tmux-zoomed)
    (tmux-toggle-zoom))
  )

(defun tmux-toggle-zoom ()
  (interactive)
  (tmux-pane-0-run-command "resize-pane" "-Z"))

(defun tmux-window-exist (window-num)
  (tmux-get-list-entry window-num "list-windows"))

(setq capture-number 0)

(defun tmux-capture-pane (&optional arg)
  (interactive "P")
  (let ((buf-name "tmux-capture")
        buf)
    (tmux-zoom-pane-0)
    (when (get-buffer buf-name) (kill-buffer buf-name))
    (switch-to-buffer (setq buf (get-buffer-create buf-name)))
    (delete-other-windows)
    (if (equal arg '(4))
        (funcall 'call-process "tmux" nil buf nil "capture-pane" "-t" "0" "-p" "-S" "-5000")
      (funcall 'call-process "tmux" nil buf nil "capture-pane" "-t" "0" "-e" "-p" "-S" "-500")
      (xterm-color-colorize-buffer))
    (setq truncate-lines 1)
    (force-mode-line-update)
    (goto-char (point-min))
    (goto-char (point-max))
    (backward-char)
    (write-file (concat "~/log/tmux-capture-" (number-to-string capture-number)))
    (if (= capture-number 9)
        (setq capture-number 0)
    (setq capture-number (+ 1 capture-number)))
    ))

(defun tmux-ctrl-c ()
  (interactive)
  (tmux-send-key "C-c")
  )

(defun tmux-ctrl-d ()
  (interactive)
  (tmux-send-key "C-d")
  )

(defun tmux-ctrl-m ()
  (interactive)
  (tmux-send-key "C-m")
  )

(defun tmux-q ()
  (interactive)
  (tmux-send-key "q")
  )

(defun tmux-ctrl-z ()
  (interactive)
  (tmux-send-key "C-z")
  )

(defun tmux-space ()
  (interactive)
  (tmux-send-key " "))

(defun tmux-n ()
  (interactive)
  (tmux-run-key "n"))

(defun tmux-y ()
  (interactive)
  (tmux-run-key "y"))

(defun tmux-clear-pane ()
  (interactive)
  (tmux-send-key "-R" "C-l")
  (tmux-pane-0-run-command "clear-history")
  )

(defun tmux-login-type ()
  (replace-regexp-in-string "'" "" (tmux-display-message "#{pane_current_command}" "1")))

(defun tmux-cd-default-directory ()
  (interactive)
  (let (vec method user host dir)
    (if (not (file-remote-p default-directory))
        (tmux-run-key "cd" " " default-directory)
      (setq vec (tramp-dissect-file-name default-directory)
            method (nth 1 vec)
            user (nth 2 vec)
	        host (nth 4 vec)
            dir (nth 6 vec))
      (cond
       ((or (equal method "ssh") (equal method "scp"))
        (if (equal "ssh" (tmux-login-type))
            (tmux-run-key "cd" (nth 6 vec))
          (tmux-run-key "ssh " user "@" host " -t \"cd " dir " ; bash --login\"" )))
       ((equal method "docker")
        (if (equal "docker" (tmux-login-type))
            (tmux-run-key (concat "cd" " " dir))
          (tmux-run-key
           "docker exec" " -u " user " -w " dir " -e COLUMNS=\"`tput cols`\" -e LINES=\"`tput lines`\" -ti "
           host " bash"))
        )
       (t
        (message "unknown tramp method")
        )))))

(defun tmux-down ()
  (interactive)
  (tmux-send-key "Down"))

(defun tmux-up ()
  (interactive)
  (tmux-send-key "Up"))

(defun tmux-command-history-prev ()
  (interactive)
  (tmux-up))

(defun tmux-command-history-next ()
  (interactive)
  (tmux-down)
  )

(defun  tmux-ls ()
  (interactive)
  (tmux-run-key "ls -lrt"))

(defun tmux-pwd ()
  (interactive)
  (tmux-run-key "pwd")
)

(defun tmux-home-dir ()
  (interactive)
  (tmux-run-key "cd")
  )

(defun tmux-last-dir ()
  (interactive)
  (tmux-run-key "cd -")
  )

(defun tmux-up-dir ()
  (interactive)
  (tmux-run-key "%" "cd .."))

(defun tmux-begin-cmd-history ()
  (tmux-run-key "history 200")
  (tmux-begin-copy-mode))

(defun tmux-run-a-history-cmd ()
  (interactive)
  (let ((history-number (read-number "history number: ")))
    (tmux-quit-copy-mode)
    (tmux-run-key (concat "!" (number-to-string history-number)))))

(setq tmux-pane-0-not-zoomed nil)

(defun tmux-begin-copy-mode ()
  (unless (tmux-pane-0-zoomed)
    (setq tmux-pane-0-not-zoomed t)
    (tmux-zoom-pane-0))
  (tmux-pane-0-run-command "copy-mode"))

(defun tmux-quit-copy-mode ()
  (interactive)
  (tmux-send-key "q")
  (if tmux-pane-0-not-zoomed
      (tmux-unzoom-pane-0))
)

(defun tmux-page-up ()
  (interactive)
  (tmux-send-key "PageUp"))

(defun tmux-page-down ()
  (interactive)
  (tmux-send-key "PageDown"))

(defun tmux-halfpage-up ()
  (interactive)
  (tmux-send-key "M-Up"))

(defun tmux-halfpage-down ()
  (interactive)
  (tmux-send-key "M-Down"))

(defun tmux-copy-mode-down ()
  (interactive)
  (tmux-send-key "C-Down"))

(defun tmux-copy-mode-up ()
  (interactive)
  (tmux-send-key "C-Up"))

(defun tmux-kill-pane ()
  (interactive)
  (let ((pane-num (tmux-select-pane-num)))
    (tmux-run-command "kill-pane" "-t" pane-num)
  ))

(defun tmux-resize-pane (directory)
  (tmux-run-command "resize-pane" "-t" tmux-selected-pane directory))

(defun tmux-resize-pane-up ()
  (interactive)
  (tmux-resize-pane "-U"))

(defun tmux-resize-pane-down ()
  (interactive)
  (tmux-resize-pane "-D"))

(defun tmux-resize-pane-left ()
  (interactive)
  (tmux-resize-pane "-L"))

(defun tmux-resize-pane-right ()
  (interactive)
  (tmux-resize-pane "-R"))

(defun tmux-new-window ()
  (interactive)
  (tmux-run-command "new-window")
  )

(defun tmux-kill-window ()
  (interactive)
  (tmux-run-command "kill-window")
  )

(defun tmux-split-window (direction)
  (tmux-run-command "split-window" "-t" tmux-selected-pane direction)
  (tmux-pane-0-run-command "select-pane")
  )

(defun tmux-split-window-horizontal ()
  (interactive)
  (tmux-split-window "-h")
  )

(defun tmux-split-window-vertical ()
  (interactive)
  (tmux-split-window "-v")
  )

(defun tmux-last-window (&optional keep-display)
  (interactive "P")
  (tmux-run-command "last-window"))

(defun tmux-next-window (&optional keep-display)
  (interactive "P")
  (tmux-run-command "next-window")
  (unless keep-display
    (tmux-emacs-frame-show-current-buffer (tmux-window-id))))

(defun tmux-select-window (window-id &optional keep-display)
    (unless keep-display
        (tmux-emacs-frame-show-current-buffer window-id))
    (tmux-run-command "select-window" "-t" window-id))

(defun tmux-select-window-0 (&optional arg)
  (interactive "P")
  (tmux-select-window "0" arg)
  )

(defun tmux-select-window-1 (&optional arg)
  (interactive "P")
  (tmux-select-window "1" arg)
  )

(defun tmux-select-window-2 (&optional arg)
  (interactive "P")
  (tmux-select-window "2" arg)
  )

(defun tmux-select-window-3 (&optional arg)
  (interactive "P")
  (tmux-select-window "3" arg)
  )

(defun tmux-select-window-4 (&optional arg)
  (interactive "P")
  (tmux-select-window "4" arg)
  )

(defun tmux-select-window-5 (&optional arg)
  (interactive "P")
  (tmux-select-window "5" arg)
  )

(defun tmux-select-window-6 (&optional arg)
  (interactive "P")
  (tmux-select-window "6" arg)
  )

(defun tmux-select-window-7 (&optional arg)
  (interactive "P")
  (tmux-select-window "7" arg)
  )

(defun tmux-select-window-8 (&optional arg)
  (interactive "P")
  (tmux-select-window "8" arg)
  )

(defun tmux-select-window-9 (&optional arg)
  (interactive "P")
  (tmux-select-window "9" arg)
  )

(defun tmux-rename-window ()
  (interactive)
  (tmux-run-command "rename-window" (read-string "New name: ")))

(setq tmux-selected-pane "0")

(defun tmux-select-pane-num ()
  (let* ((num-list (tmux-get-pane-or-window-number-list t))
         (prompt "Select Pane:"))
    (my/tmux-select-number num-list prompt)))

(defun tmux-select-pane ()
  (interactive)
  (tmux-display-pane-numbers)
  (tmux-run-command "select-pane" "-t" (tmux-select-pane-num)))

(defun tmux-select-pane-0 ()
  (interactive)
  (tmux-pane-0-run-command "select-pane"))

(defun tmux-swap-pane ()
  (interactive)
  (let ((num-list (tmux-get-pane-or-window-number-list t))
        (prompt "Select Pane:"))
    (if (<= (length num-list) 1)
        (error "this command is used for swapping two terminal panes"))
    (if (= (length num-list) 2)
        (tmux-pane-0-run-command "swap-pane" "-s" "1")
      (tmux-display-pane-numbers)
      (setq src-pane-id (my/tmux-select-number (cddr num-list) prompt))
      (tmux-pane-0-run-command "swap-pane" "-s" src-pane-id))
    (tmux-pane-0-run-command "select-pane")
    ))

(defun tmux-tail-this-file ()
  (interactive)
  (let ((fn (if (file-remote-p default-directory)
                (tramp-file-name-localname  (tramp-dissect-file-name (dired-get-filename)))
              (dired-get-filename))))
    (tmux-run-key (concat "tail -f " fn))
    ))


(provide 'tmux-head)
