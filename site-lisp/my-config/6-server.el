;;; 6-server.el --- server settings


;;; Code:


(unless (daemonp)
  (setq server-use-tcp t)
  (setq server-host "localhost")
  ;;(setq server-port 40987)
  )


;; 6-server.el ends here
