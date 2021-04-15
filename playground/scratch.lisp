(defun open-bluetooth-socket (&key local-mac remote-mac reset-device?)
  (when (and local-mac
             remote-mac)
    (error "~S was called with both LOCAL-MAC and REMOTE-MAC" 'open-bluetooth-socket))
  (bind ((device-id (cond
                      (remote-mac
                       (c-with ((device-address bluez:bdaddr-t))
                         (c-fun/rc bluez:str2ba remote-mac device-address)
                         (c-fun/rc bluez:hci-get-route device-address)))
                      (local-mac
                       (c-fun/rc bluez:hci-devid local-mac)))))
    (when reset-device?
      (bluez:hci/reset-device device-id)
      (format t "Device was reset successfully~%"))
    (c-fun/rc bluez:hci-open-dev device-id)))

(defmacro with-open-bluetooth-socket ((fd-var &key local-mac remote-mac reset-device?) &body body)
  `(let ((,fd-var (open-bluetooth-socket :local-mac ,local-mac :remote-mac ,remote-mac)))
     (unwind-protect
          (progn
            ,@body)
       (c-fun/rc bluez:hci-close-dev ,fd-var))))


(defun test ()
  (with-open-bluetooth-socket (device-id device-fd
                                :local-device "00:1A:7D:DA:71:13"
                               ;; :local-device "hci2"
                               ;; :remote-device "00:07:80:2E:CB:43"
                               )
    (bluez:hci/reset-device device-id)
    (format t "Device was reset successful~%")
    (progn
      (format t "device-fd is: ~S, device name is: ~S~%" device-fd (bluez:hci-device-name device-id))
      (c-fun/rc bluez:hci-le-set-scan-parameters device-fd 1 #x10 #x10 0 0 1000)
      (c-fun/rc bluez:hci-le-set-scan-enable device-fd 1 1 1000)
      (let ((buffer-size bluez:+hci-max-event-size+))
        (c-with ((buffer bluez:uint8-t :count buffer-size)
                 (original-filter bluez:hci-filter)
                 (original-filter-struct-size bluez:socklen-t :value (autowrap:sizeof 'bluez:hci-filter))
                 (new-filter bluez:hci-filter))
          (format t "backing up the filter, (original-filter-struct-size &) ~S~%" (original-filter-struct-size &))
          (c-fun/rc bluez:getsockopt device-fd bluez:+sol-hci+ bluez:+hci-filter+ (original-filter &) (original-filter-struct-size &))
          ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr original-filter) (autowrap:sizeof 'bluez:hci-filter))))
          (bluez:hci-filter/initialize-for-le-scanning new-filter)
          ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr new-filter) (autowrap:sizeof 'bluez:hci-filter))))
          (c-fun/rc bluez:setsockopt device-fd bluez:+sol-hci+ bluez:+hci-filter+ (new-filter &) (autowrap:sizeof 'bluez:hci-filter))
          (format t "filter set, calling read now~%")
          (loop
            :for bytes-read = (bluez.ffi:read device-fd (buffer &) buffer-size)
            :while (or (>= bytes-read 0)
                       (eql autowrap:errno bluez:+eintr+)
                       (eql autowrap:errno bluez:+ewouldblock+))
            :do
            (cond
              ((plusp bytes-read)
               ;;(format t "~&Read ~S bytes " bytes-read)
               (print-le-advertising-event (buffer &)))
              ((eql autowrap:errno bluez:+ewouldblock+)
               ;;(format t "tick~%")
               (sleep 0.1))))))
      (format t "Body finished~%")))
  (format t "Exiting~%"))
