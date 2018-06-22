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
(global-set-key (kbd "M-g c") 'capitalize-dwim)
(global-set-key (kbd "M-g d") 'gdb)
(global-set-key (kbd "M-g g") 'goto-line)
(global-set-key (kbd "M-g m") 'magit-status)
(global-set-key (kbd "M-g s") 'ispell-buffer)

(require 'whitespace)
(setq whitespace-style '(face tabs trailing space-after-tab space-before-tab empty))
(setq whitespace-global-modes '(not go-mode))
(global-whitespace-mode t)

(setq org-replace-disputed-keys t)

;; sadly I sometimes have to edit java
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4)))

(defun other-window-backward ()
  (interactive)
  (other-window -1))

(defun exwm-workspace-next ()
  (interactive)
  (let ((next-index (mod (+ exwm-workspace-current-index 1)
                         (+ exwm-workspace-number 1))))
    (exwm-workspace-switch next-index)))

(defun exwm-workspace-prev ()
  (interactive)
  (let ((prev-index (mod (- exwm-workspace-current-index 1)
                         (+ exwm-workspace-number 1))))
    (exwm-workspace-switch prev-index)))

(defun use-exwm ()
      (require 'exwm)
      (require 'exwm-config)
      (require 'exwm-randr)
      (exwm-config-default)
      (exwm-randr-enable)
      (ido-mode nil)
      (setq exwm-workspace-number 5)
      (ansi-term "/bin/bash")

      (exwm-input-set-key (kbd "<s-tab>") 'exwm-workspace-next)
      (exwm-input-set-key (kbd "<s-iso-lefttab>") 'exwm-workspace-prev)
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

      (require 'exwm-randr-control)
      (exwm-randr-control-init)

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
                  (if (eq exwm-class-name "okular") (exwm-input-toggle-keyboard)))))


;; on Macs, home and end go to the beginning and end of the file by default
;; so make them behave as expected
(if (eq window-system 'ns)
    (progn
      (define-key global-map [home] 'move-beginning-of-line)
      (define-key global-map [end] 'move-end-of-line)))

(if (eq window-system 'x)
    (use-exwm))

(require 'bind-key)
(bind-key* "<C-tab>" 'other-window)
(bind-key* "<C-S-tab>" 'other-window-backward)
(bind-key* "<C-iso-lefttab>" 'other-window-backward)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

;; the go tools in particular rely on a lot of things being on the path
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defun gofmt-go-mode-buffers ()
  (when (eq major-mode 'go-mode)
    (gofmt-before-save)))

(add-hook 'before-save-hook 'gofmt-go-mode-buffers)

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-*") 'pop-tag-mark)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(battery-mode-line-format " [%B %t rem] ")
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(cua-enable-cua-keys t)
 '(cua-mode t nil (cua-base))
 '(custom-buffer-indent 2)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("1d9401cbf6d663adc797e83917b1a3d1cf916990acb75d11679546bc7dfd9153" "31086c59cd60bef4d9ea4997caa79ade74139abe92b05c2cc30095bb62150189" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(exec-path
   (quote
    ("/Users/iwakeling/bin/" "/usr/local/bin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/Library/TeX/texbin/" "/usr/local/go/bin/" "/opt/X11/bin/" "/usr/local/go/bin/" "/Applications/Emacs.app/Contents/MacOS/libexec/")))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(latex-run-command "pdflatex")
 '(org-support-shift-select (quote always))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (bind-key exwm go-mode markdown-mode solarized-theme exec-path-from-shell magit)))
 '(pop-up-windows nil)
 '(python-indent-offset 2)
 '(show-paren-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
