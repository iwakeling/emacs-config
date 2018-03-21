;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)

(autoload 'scad-mode "scad-mode" "A major mode for editing OpenSCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))

(defun compile-in-makefile-directory ()
  (interactive)
  (let ((default-directory
	  (locate-dominating-file (buffer-file-name) "Makefile")))
    (call-interactively 'compile)))

(global-set-key (kbd "M-g b") 'compile-in-makefile-directory)
(global-set-key (kbd "M-g d") 'gud-gdb)
(global-set-key (kbd "M-g g") 'goto-line)
(global-set-key (kbd "M-g c") 'capitalize-dwim)
(global-set-key (kbd "M-g s") 'ispell-buffer)

;; if not using exwm, this is useful
;; exwm version below
;; TODO: get the expanded version from work
;; (global-set-key (kbd "<C-tab>") 'other-window)

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(exwm-config-default)
(exwm-randr-enable)
(ido-mode nil)
(setq exwm-workspace-number 5)
(ansi-term "/bin/bash")

(exwm-input-set-key (kbd "<s-tab>") 'switch-to-buffer)
(exwm-input-set-key (kbd "<s-iso-lefttab>") 'previous-buffer)
(exwm-input-set-key (kbd "<C-tab>") 'other-window)
(exwm-input-set-key (kbd "s-p")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "pactl" nil "pactl set-sink-volume 0 +5%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "pactl" nil "pactl set-sink-volume 0 -5%")))
(exwm-input-set-key (kbd "<XF86AudioMute>")
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "pactl" nil "pactl set-sink-mute 0 toggle")))
(exwm-input-set-key (kbd "<XF86Display>")
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "xrandr" nil "/home/ian/bin/toggle-external-display")))

(add-hook 'exwm-randr-screen-change-hook
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "xrandr" nil "/home/ian/bin/toggle-external-display")))

;; TODO: assign workspaces to displays, e.g.
;; (setq exwm-randr-workspace-output-plist '(1 "LVDS1"))
;; perhaps using "s-!", "s-\"" etc

;; Workspace assignments for X window buffers
(require 'subr-x)  ;; Because of when-let

(defvar exwm-workspace-window-assignments
  '(("Firefox" . 3)
    ("Thunderbird" . 4))
  "An alist of window classes and which workspace to put them on.")

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when-let ((target (cdr (assoc exwm-class-name exwm-workspace-window-assignments))))
		      (exwm-workspace-move-window target)
		      (exwm-input-toggle-keyboard))))

(add-hook 'exwm-manage-finish-hook
	  (lambda ()
	    (if (eq exwm-class-name "okular") (exwm-input-toggle-keyboard))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%B %t rem] ")
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-buffer-indent 2)
 '(custom-enabled-themes (quote (wombat)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(latex-run-command "pdflatex")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages (quote (exwm magit)))
 '(python-indent-offset 2)
 '(show-paren-mode t)
 '(standard-indent 2)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
