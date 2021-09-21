(provide 'tcp-server)

(defun tcp-server-ip-addr-from-interface (interface)
  (let ((addr-data (shell-command-to-string
                    (format "ip address show dev %s"
                            interface))))
    (if (string-match
         "^[[:space:]]*inet \\([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\)"
         addr-data)
        (match-string 1 addr-data))))
;; else try inet6?

(defun tcp-server-listen-addr (ip-addr remote)
  (message "starting tcp server on %s and sharing with %s"
           ip-addr
           remote)
  (setq server-host ip-addr)
  (setq server-use-tcp t)
  (server-mode)
  (message (shell-command-to-string
            (format "scp ~/.emacs.d/server/server %s:/.emacs.d/server.%s"
                    remote
                    (system-name)))))

(defun tcp-server-listen (interface remote)
  (interactive "sInterface (e.g. wlp1s0 or tun0):\nsRemote Machine:")
  (let ((ip-addr (tcp-server-ip-addr-from-interface interface)))
    (if (not (equal ip-addr nil))
        (tcp-server-listen-addr ip-addr remote)
      (message "no address for interface %s" interface))))
