(in-package :bt-mqtt-gateway)

(def function executable-toplevel ()
  (test)
  ;;+process-return-code/no-error+
  0)

(deftype fd ()
  '(integer 1))

(defun (setf fd-nonblocking-p) (enabled fd)
  (check-type fd fd)
  (let* ((current-flags (c-fun/rc bluez:fcntl fd bluez:+f-getfl+))
         (new-flags (if enabled
                        (logior current-flags bluez:+o-nonblock+)
                        (logand current-flags (lognot bluez:+o-nonblock+)))))
    (unless (eql current-flags new-flags)
      (format t "Setting fd ~S flags to ~S~%" fd new-flags)
      (c-fun/rc bluez:fcntl fd bluez:+f-setfl+ :int new-flags)))
  (values))

(defun test ()
  (let (
        (mac "00:1A:7D:DA:71:13") ; LE enabled
        ;;(mac "C0:CB:38:AD:A2:61")
        ;;(mac "hci0")
        )
    (c-with ((device-address bluez:bdaddr-t))
      (c-fun/rc bluez:str2ba mac device-address)
      (let* ((device-id (c-fun/rc bluez:hci-devid mac))
             ;;(device-id (hci-get-route device-address))
             )
        (bluez:hci/reset-device device-id)
        (format t "Device was reset successful~%")
        (let ((device-fd (c-fun/rc bluez:hci-open-dev device-id)))
          (unwind-protect
               (progn
                 (format t "device-fd is: ~S, mac is: ~S, device name is: ~S~%" device-fd mac (bluez:hci-device-name device-id))
                 (c-fun/rc bluez:hci-le-set-scan-parameters device-fd 1 #x10 #x10 0 0 1000)
                 (c-fun/rc bluez:hci-le-set-scan-enable device-fd 1 1 1000)
                 (setf (fd-nonblocking-p device-fd) t)
                 (let ((buffer-size bluez:+hci-max-event-size+))
                   (c-with ((buffer bluez:uint8-t :count buffer-size)
                            (original-filter bluez:hci-filter)
                            (original-filter-struct-size bluez:socklen-t :value (autowrap:sizeof 'bluez:hci-filter))
                            (new-filter bluez:hci-filter))
                     (format t "backing up the filter, (original-filter-struct-size &) ~S~%" (original-filter-struct-size &))
                     (c-fun/rc bluez:getsockopt device-fd bluez:+sol-hci+ bluez:+hci-filter+ (original-filter &) (original-filter-struct-size &))
                     (format t "1~%")
                     ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr original-filter) (autowrap:sizeof 'bluez:hci-filter))))
                     (bluez:hci-filter/initialize-for-scanning new-filter)
                     (format t "2~%")
                     ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr new-filter) (autowrap:sizeof 'bluez:hci-filter))))
                     (c-fun/rc bluez:setsockopt device-fd bluez:+sol-hci+ bluez:+hci-filter+ (new-filter &) (autowrap:sizeof 'bluez:hci-filter))
                     (format t "filter set, calling read now~%")
                     (loop
                       :for bytes-read = (bluez.ffi:read device-fd (buffer &) buffer-size)
                       :while (or (>= bytes-read 0)
                                  (eql autowrap:errno bluez:+ewouldblock+))
                       :do
                       (cond
                         ((plusp bytes-read)
                          (format t "Read ~S bytes~%" bytes-read))
                         ((eql autowrap:errno bluez:+ewouldblock+)
                          ;;(format t "tick~%")
                          (sleep 0.1))))))
                 (format t "Body finished~%"))
            (c-fun/rc bluez:hci-close-dev device-fd))))))
  (format t "Exiting~%"))
