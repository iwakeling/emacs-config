(provide 'exwm-randr-control)

(defvar exwm-randr-control-state 'off
  "current state of xrandr")

(defvar exwm-randr-control-base-display "LVDS1"
  "the base display to arrange other displays relative to")

(defun exwm-randr-control-init ()
  ;; exwm-randr-workspace-ouput-plist has to be initialised to something
  ;; or plist-put doesn't work
  (setq exwm-randr-workspace-output-plist '(0 exwm-randr-control-base-display))
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
			(exwm-randr-control-move-workspace-to-nth-display 2))))

(defun exwm-randr-control-maybe-toggle ()
  (interactive)
  (let ((active-displays (exwm-randr-get-active-displays)))
    (if (equal (length active-displays) 0)
	(message "No external displays connected")
      (exwm-randr-control-toggle))))

(defun exwm-randr-control-toggle ()
  (let ((active-displays (exwm-randr-get-active-displays))
	(state-machine '((off . mirrored)
			 (mirrored . split)
			 (split . off)))
	(prev-display exwm-randr-control-base-display))
    (dolist (display active-displays)
      (cond ((eq exwm-randr-control-state 'off)
	     (exwm-randr-control-mirror-display display))
	    ((eq exwm-randr-control-state 'mirrored)
	     (exwm-randr-control-split-display display prev-display))
	    ((eq exwm-randr-control-state 'split)
	     (exwm-randr-control-switch-off-display display)))
      (setq prev-display display))
    (when-let ((next-state (cdr (assoc exwm-randr-control-state state-machine))))
      (message "xrandr was %s, now %s" exwm-randr-control-state next-state)
      (setq exwm-randr-control-state next-state))))

(defun exwm-randr-get-active-displays ()
  (let ((display-data (shell-command-to-string "xrandr"))
      (pos 0)
      (matches ()))
    (while (string-match "^\\(.*\\) connected" display-data pos)
      (let ((display (match-string 1 display-data)))
	(unless (equal display exwm-randr-control-base-display)
	    (push display matches)))
    (setq pos (match-end 0)))
  (setq matches (reverse matches))))

(defun exwm-randr-control-mirror-display (display)
  (shell-command-to-string
   (format "xrandr --output %s --auto --same-as %s"
	   display
	   exwm-randr-control-base-display)))

(defun exwm-randr-control-split-display (display prev-display)
  (shell-command-to-string
   (format "xrandr --output %s --auto --above %s" display prev-display)))

(defun exwm-randr-control-switch-off-display (display)
  (shell-command-to-string
   (format "xrandr --output %s --off" display)))

(defun exwm-randr-control-move-workspace-to-nth-display (n)
  (if (eq exwm-randr-control-state 'split)
      (let ((active-displays (exwm-randr-get-active-displays)))
	(exwm-randr-control-move-workspace-to-display
	 (nth n active-displays)))
    (message "xrandr state is not split")))

(defun exwm-randr-control-move-workspace-to-display (display)
  (message "putting workspace %s on display %s"
	   exwm-workspace-current-index
	   display)
  (plist-put exwm-randr-workspace-output-plist
	     exwm-workspace-current-index
	     display)
  (exwm-randr--refresh))
