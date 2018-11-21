(provide 'exwm-randr-control)

(defvar exwm-randr-control-base-display "LVDS1"
  "the base display to arrange other displays relative to")

(defvar exwm-randr-control-display-modes '()
  "overrides for output display mode, e.g. ((HDMI1 . --mode 1920x1080)(HDMI2 . --mode 1280x1024))")

(defun exwm-randr-control-init ()
  (exwm-randr-control-get-primary-display)
  ;; exwm-randr-workspace-ouput-plist has to be initialised to something
  ;; or plist-put doesn't work
  (setq exwm-randr-workspace-output-plist '(0 exwm-randr-control-base-display))

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (exwm-randr-control-disable-disconnected-displays)))

  (exwm-input-set-key (kbd "<XF86Display>") 'exwm-randr-control-maybe-toggle)
  (exwm-input-set-key (kbd "s-a")
                      (lambda ()
                        (interactive)
                        (exwm-randr-control-move-workspace-to-display
                         exwm-randr-control-base-display)))
  (exwm-input-set-key (kbd "s-s")
                      (lambda ()
                        (interactive)
                        (exwm-randr-control-move-workspace-to-nth-display 0)))
  (exwm-input-set-key (kbd "s-d")
                      (lambda ()
                        (interactive)
                        (exwm-randr-control-move-workspace-to-nth-display 1)))
  (exwm-input-set-key (kbd "s-f")
                      (lambda ()
                        (interactive)
                        (exwm-randr-control-move-workspace-to-nth-display 2)))
  (exwm-input-set-key (kbd "s-m") 'exwm-randr-control-set-mode)
  (exwm-input-set-key (kbd "s-l") 'exwm-randr-control-show-displays))

(defun exwm-randr-control-maybe-toggle ()
  (interactive)
  (let ((display-data (exwm-randr-control-display-data)))
    (if (equal
         (length (exwm-randr-control-get-connected-displays display-data))
         0)
        (message "No external displays connected")
      (exwm-randr-control-toggle display-data))))

(defun exwm-randr-control-show-displays ()
  (interactive)
  (let ((xrandr-buf (get-buffer-create "*xrandr*")))
    (with-current-buffer xrandr-buf
      (goto-char (point-max))
      (insert (exwm-randr-control-display-data)))
    (display-buffer xrandr-buf)))

(defun exwm-randr-control-set-mode (display mode)
  (interactive "sDisplay (e.g. HDMI1):\nsMode (e.g. --mode 1920x1080)")
  (let ((cell (assoc display exwm-randr-control-display-modes)))
    (if (eq cell nil)
        (add-to-list 'exwm-randr-control-display-modes (cons display mode))
      (setcdr cell mode)))
  (exwm-randr--refresh))

(defun exwm-randr-control-toggle (display-data)
  (let ((new-state (exwm-randr-control-next-state display-data))
        (connected-displays (exwm-randr-control-get-connected-displays display-data))
        (prev-display exwm-randr-control-base-display))
    (dolist (display connected-displays)
      (cond
       ((equal new-state "split")
        (exwm-randr-control-split-display display prev-display))
       ((equal new-state "mirrored")
        (exwm-randr-control-mirror-display display))
       ((equal new-state "off")
        (exwm-randr-control-switch-off-display display)))
      (setq prev-display display))
    (message "xrandr state now %s" new-state)))

(defun exwm-randr-control-display-data ()
  (shell-command-to-string "xrandr"))

(defun exwm-randr-control-count-displays-at-zero-zero (display-data)
  (let ((pos 0)
        (matches 0))
    (while (string-match "^\\(.*\\) connected\\(?: primary\\)? [0-9]+x[0-9]+[+]0[+]0" display-data pos)
      (setq matches (+ matches 1))
      (setq pos (match-end 0)))
    matches))

(defun exwm-randr-control-count-active-displays (display-data)
  (let ((pos 0)
        (matches 0))
    (while (string-match "^\\(.*\\) connected\\(?: primary\\)? [0-9]+x[0-9]+" display-data pos)
      (setq matches (+ matches 1))
      (setq pos (match-end 0)))
    matches))

(defun exwm-randr-control-is-split (display-data)
  (not (= (exwm-randr-control-count-active-displays display-data)
          (exwm-randr-control-count-displays-at-zero-zero display-data))))

(defun exwm-randr-control-is-mirrored (display-data)
  (and (> (exwm-randr-control-count-active-displays display-data) 1)
       (= (exwm-randr-control-count-active-displays display-data)
          (exwm-randr-control-count-displays-at-zero-zero display-data))))

(defun exwm-randr-control-get-connected-displays (display-data)
  (let ((pos 0)
        (matches ()))
    (while (string-match "^\\(.*\\) connected" display-data pos)
      (let ((display (match-string 1 display-data)))
        (unless (equal display exwm-randr-control-base-display)
          (push display matches)))
      (setq pos (match-end 0)))
    (setq matches (reverse matches))))

(defun exwm-randr-control-next-state(display-data)
  (cond
   ((and (not (exwm-randr-control-is-split display-data))
         (not (exwm-randr-control-is-mirrored display-data)))
    "split")
   ((exwm-randr-control-is-split display-data)
    "mirrored")
   ((exwm-randr-control-is-mirrored display-data)
    "off")
   ))

(defun exwm-randr-control-get-primary-display ()
  (let ((display-data (shell-command-to-string "xrandr"))
        (pos 0))
    (if (string-match "^\\(.*\\) connected primary" display-data pos)
      (let ((display (match-string 1 display-data)))
        (setq exwm-randr-control-base-display display)))))

(defun exwm-randr-control-display-mode (display)
  (alist-get display exwm-randr-control-display-modes "--auto" nil `equal))

(defun exwm-randr-control-mirror-display (display)
  (shell-command-to-string
   (format "xrandr --output %s %s --same-as %s"
           display
           (exwm-randr-control-display-mode display)
           exwm-randr-control-base-display)))

(defun exwm-randr-control-split-display (display prev-display)
  (shell-command-to-string
   (format "xrandr --output %s %s --above %s"
           display
           (exwm-randr-control-display-mode display)
           prev-display)))

(defun exwm-randr-control-switch-off-display (display)
  (shell-command-to-string
   (format "xrandr --output %s --off" display)))

(defun exwm-randr-control-move-workspace-to-nth-display (n)
  (let ((display-data (exwm-randr-control-display-data)))
    (if (exwm-randr-control-is-split display-data)
        (exwm-randr-control-move-workspace-to-display
         (nth n (exwm-randr-control-get-connected-displays display-data)))
      (message "xrandr state is not split"))))

(defun exwm-randr-control-move-workspace-to-display (display)
  (message "putting workspace %s on display %s"
     exwm-workspace-current-index
      display)
  (plist-put exwm-randr-workspace-output-plist
       exwm-workspace-current-index
       display)
  (exwm-randr--refresh))

(defun exwm-randr-control-disable-disconnected-displays ()
  (let ((display-data (shell-command-to-string "xrandr"))
        (pos 0))
    (while (string-match "^\\(.*\\) disconnected" display-data pos)
      (let ((display (match-string 1 display-data)))
        (unless (equal display exwm-randr-control-base-display)
          (exwm-randr-control-switch-off-display display)))
      (setq pos (match-end 0)))
    (let ((connected-displays (exwm-randr-control-get-connected-displays display-data)))
      (if (equal (length connected-displays) 0)
          (progn
            ;; clear the output setup so things don't appear immediately next
            ;; time a display is plugged in
            (setq exwm-randr-workspace-output-plist '(0 exwm-randr-control-base-display))
            (exwm-randr--refresh)
            (message "all displays disconnected, xrandr state now off"))))))
