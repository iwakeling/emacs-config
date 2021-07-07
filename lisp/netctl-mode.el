;; Major mode for interacting with netctl

(provide 'netctl-mode)

(defun netctl-keymap ()
  '(("g" . 'netctl-mode-refresh)
    ("s" . 'netctl-mode-start)
    ("k" . 'netctl-mode-kill)
    ("c" . 'netctl-mode-change)))

(defvar netctl-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (entry (netctl-keymap))
      (define-key map (car entry) (eval (cdr entry))))
    map)
  "The keymap entries")

(defvar netctl-mode-buf nil
  "The one and only netctl buffer")

(defvar netctl-mode-list-end (point-min)
  "Tracks the position of the end of the list of profiles")

(defvar netctl-mode-last-msg ""
  "The output from the last netctl cmd")

(define-derived-mode netctl-mode special-mode
  "Netctl"
  "Major mode for netctl."
  (netctl-mode-refresh))

(defun netctl-mode-update-display (list)
  (let ((buffer-read-only nil)
        (cur-point (point))
        (profile-pos 0))
    (erase-buffer)
    (goto-char (point-min))
    (dolist (entry (netctl-keymap))
      (insert (car entry))
      (insert ": ")
      (insert (documentation (eval (cdr entry))))
      (insert "\n"))
    (insert "\n\n-------------------------\nKnown Network Profiles:\n\n")
    (insert list)
    (setq netctl-mode-list-end (point))
    (insert "\n\n-------------------------\nNetwork Profile Status:\n\n")
    (while (string-match "\\*\\(.*\\).*" list profile-pos)
      (let ((profile-name (match-string 1 list)))
        (insert (shell-command-to-string
                 (format "netctl status %s"
                         profile-name))))
      (setq profile-pos (match-end 0)))
    (setq profile-pos 0)
    (while (string-match "\\+\\(.*\\).*" list profile-pos)
      (let ((profile-name (match-string 1 list)))
        (insert (format "connecting to %s ..." profile-name)))
      (insert (format "%s" netctl-mode-last-msg))
      (setq profile-pos (match-end 0)))
    (goto-char cur-point)))

(defun netctl-mode-do-cmd (cmd profile-name)
  (setq netctl-mode-last-msg
        (shell-command-to-string
         (format "sudo netctl %s %s" cmd profile-name))))

(defun netctl-mode-run-netctl (cmd)
  (let ((profile-line (thing-at-point 'line t)))
    (if (< (point) netctl-mode-list-end)
        (if (string-match "\\*?[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*" profile-line)
            (let ((profile-name (match-string 1 profile-line))
                  (buffer-read-only nil))
              (setq netctl-mode-last-msg (format "running `netctl %s %s` ..." cmd profile-name))
              (kill-region netctl-mode-list-end (point-max))
              (goto-char netctl-mode-list-end)
              (insert "\n\n")
              (insert netctl-mode-last-msg)
              (run-at-time 0.1 nil 'netctl-mode-do-cmd cmd profile-name))
          (message (format "unrecognised profile %s" profile-line)))
      (message "no profile at point"))))

(defun netctl-mode-wait-start ()
  (with-current-buffer netctl-mode-buf
    (let ((netctl-list (shell-command-to-string "netctl list")))
      (netctl-mode-update-display netctl-list)
      (if (not (string-match "\\*\\(.*\\).*" netctl-list 0))
          (run-at-time "1 sec" nil 'netctl-mode-wait-start)))))

(defun netctl-mode-refresh ()
  "Refresh the display"
  (interactive)
  (let ((netctl-list (shell-command-to-string "netctl list")))
    (netctl-mode-update-display netctl-list)))

(defun netctl-mode-start ()
  "Start the network profile at point"
  (interactive)
  (netctl-mode-run-netctl "start")
  (run-at-time "1 sec" nil 'netctl-mode-wait-start))

(defun netctl-mode-kill ()
  "Stop the network profile at point"
  (interactive)
  (netctl-mode-run-netctl "stop")
  (run-at-time "1 sec" nil 'netctl-mode-refresh))

(defun netctl-mode-change ()
  "Change to the network profile at point"
  (interactive)
  (netctl-mode-run-netctl "switch-to")
  (run-at-time "1 sec" nil 'netctl-mode-wait-start))

(defun open-netctl-mode ()
  (interactive)
  (setq netctl-mode-buf (get-buffer-create "*netctl-buf*"))
  (with-current-buffer netctl-mode-buf
    (netctl-mode))
  (display-buffer netctl-mode-buf))
