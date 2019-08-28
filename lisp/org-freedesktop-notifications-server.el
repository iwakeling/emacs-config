(provide 'org-freedesktop-notifications-server)
(require 'dbus)

(defvar ofns-next-msg-id 1 "Place to track the next message ID to use")

(defun ofns-get-capabilities()
  ;; don't support any of the optional extras for now
  (list))

(defun ofns-notify(app-name
                   replaces-id
                   app-icon
                   summary
                   body
                   actions
                   hints
                   expire-timeout)
  ;; just display in mini-buffer
  ;; to see past messages, just look in *Messages*!
  (message "%s says %s" app-name summary)
  (let ((result ofns-next-msg-id))
    (setq ofns-next-msg-id (+ result 1))
    result))

(defun ofns-close-notification(id)
  ;; don't do anything for now
  (:ignore))

(defun ofns-get-server-information()
  (list :string "emacs org-freedesktop-notifications-server"
        :string "Ian Wakeling"
        :string "0"
        :string "1.2"))

(defun ofns-start()
  "register the org.freedesktop.Notifications interface"
  (dbus-register-method
   :session
   "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications"
   "GetCapabilities"
   'ofns-get-capabilities)

  (dbus-register-method
   :session
   "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications"
   "Notify"
   'ofns-notify)

  (dbus-register-method
   :session
   "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications"
   "CloseNotification"
   'ofns-close-notification)

  (dbus-register-method
   :session
   "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications"
   "GetServerInformation"
   'ofns-get-server-information)
  )
