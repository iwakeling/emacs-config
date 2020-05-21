;; Major mode for interacting with netctl

(provide 'netctl-mode)

(defvar netctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'netctl-mode-refresh)
    (define-key map "s" 'netctl-mode-start)
    (define-key map "k" 'netctl-mode-kill)
    map))

(defvar netctl-mode-list-end (point-min))

(define-derived-mode netctl-mode special-mode
  "Netctl"
  "Major mode for netctl."
  (netctl-mode-refresh))

(defun netctl-mode-update-display (list)
  (let ((buffer-read-only nil)
        (cur-point (point)))
    (erase-buffer)
    (goto-char (point-min))
    (insert list)
    (setq netctl-mode-list-end (point))
    (insert "\n")
    (if (string-match "\\*\\(.*\\).*" list 0)
        (let ((profile-name (match-string 1 netctl-list)))
          (insert (shell-command-to-string
                   (format "netctl status %s"
                           profile-name)))))
    (goto-char cur-point)))

(defun netctl-mode-run-netctl (cmd)
  (let ((profile-line (thing-at-point 'line t)))
    (if (< (point) netctl-mode-list-end)
        (if (string-match "\\*?[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*" profile-line)
            (let ((profile-name (match-string 1 profile-line)))
              (message
               (shell-command-to-string
                (format "sudo netctl %s %s" cmd profile-name)))
              (netctl-mode-refresh))
          (message (format "unrecognised profile %s" profile-line)))
      (message "no profile at point"))))

(defun netctl-mode-wait-start ()
  (let ((netctl-list (shell-command-to-string "netctl list")))
    (netctl-mode-update-display netctl-list)
    (if (not (string-match "\\*\\(.*\\).*" netctl-list 0))
        (run-at-time "1 sec" nil 'netctl-mode-wait-start))))

(defun netctl-mode-refresh ()
  (interactive)
  (let ((netctl-list (shell-command-to-string "netctl list")))
    (netctl-mode-update-display netctl-list)))

(defun netctl-mode-start ()
  (interactive)
  (netctl-mode-run-netctl "start")
  (run-at-time "1 sec" nil 'netctl-mode-wait-start))

(defun netctl-mode-kill ()
  (interactive)
  (netctl-mode-run-netctl "stop"))

(defun open-netctl-mode ()
  (interactive)
  (let ((netctl-buf (get-buffer-create "*netctl-buf*")))
    (with-current-buffer netctl-buf
      (netctl-mode))
    (display-buffer netctl-buf)))
