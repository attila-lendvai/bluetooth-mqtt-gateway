;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :bt-mqtt-gateway)

(defun print-le-advertising-event (buffer)
  (let ((props (parse-le-advertising-event buffer)))
    (print (getf props :address))))

(defun parse-le-advertising-event (buffer)
  (c-let ((event hu.dwim.bluez.ffi:evt-le-meta-event :from (cffi:inc-pointer (ptr buffer) (+ 1 bluez:+hci-event-hdr-size+))))
    (when (eql (event :subevent)
               bluez:+evt-le-advertising-report+)
      (print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr buffer) 45)))
      ;;(terpri)
      (c-let ((info bluez:le-advertising-info :from (cffi:inc-pointer (event :data &) 1)))
        (append (bluez:parse-extended-inquiry-response (info :data &) (info :length))
                (list :address (bluez:bdaddr->string (info :bdaddr))))))))

(defun executable-toplevel ()
  (main-loop)
  (format t "Exiting~%")
  0)

(defun main-loop ()
  (with-hci-connection-context ()
    (bind ((hci-connection (open-hci-connection :local-device "00:1A:7D:DA:71:13"
                                                ;; :local-device "hci2"
                                                ;; :remote-device "00:07:80:2E:CB:43"
                                                ))
           ((:read-only-slots hci-device-id socket) hci-connection))
      (log.debug "Opened bluetooth ~A~%" hci-connection)
      (bluez:hci/reset-device hci-device-id)
      (format t "Device was reset successful~%")
      (progn
        (format t "socket is: ~S, device name is: ~S~%" socket (bluez:hci-device-name hci-device-id))
        (c-fun/rc bluez:hci-le-set-scan-parameters socket 1 #x10 #x10 0 0 1000)
        (c-fun/rc bluez:hci-le-set-scan-enable socket 1 1 1000)
        (let ((buffer-size bluez:+hci-max-event-size+))
          (c-with ((buffer bluez:uint8-t :count buffer-size)
                   (original-filter bluez:hci-filter)
                   (original-filter-struct-size bluez:socklen-t :value (autowrap:sizeof 'bluez:hci-filter))
                   (new-filter bluez:hci-filter))
            (format t "backing up the filter, (original-filter-struct-size &) ~S~%" (original-filter-struct-size &))
            (c-fun/rc bluez:getsockopt socket bluez:+sol-hci+ bluez:+hci-filter+ (original-filter &) (original-filter-struct-size &))
            ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr original-filter) (autowrap:sizeof 'bluez:hci-filter))))
            (bluez:hci-filter/initialize-for-le-scanning new-filter)
            ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr new-filter) (autowrap:sizeof 'bluez:hci-filter))))
            (c-fun/rc bluez:setsockopt socket bluez:+sol-hci+ bluez:+hci-filter+ (new-filter &) (autowrap:sizeof 'bluez:hci-filter))
            (format t "filter set, calling read now~%")
            (loop
              :for bytes-read = (bluez.ffi:read socket (buffer &) buffer-size)
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
        (format t "Body finished~%")))))

#+nil
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

;; topic: $aws/things/AttilaGateway/shadow/update

(cffi:defcallback mosquitto/callback/log :void ((client :pointer) (userdata :pointer) (level :int) (message :string))
  (declare (ignore client userdata))
  (format t "MSQT: ~D ~S~%" level message))

(defun mtest ()
  (mosquitto:lib-init :minimum-version '(1 4 4))
  (apply 'format t "Mosquitto lib version: ~D.~D.~D~%" (multiple-value-list (mosquitto:lib-version)))
  (mosquitto:with-new-session (:client-id "Lisp MQTT client"
                               :log-callback (cffi:callback mosquitto/callback/log)
                               :tls-certificate-authority #P"/media/store/work/iblue/install/aws/rootCA.pem"
                               :tls-client-certificate #P"/media/store/work/iblue/install/aws/9cc6b97024-certificate.pem.crt"
                               :tls-private-key #P"/media/store/work/iblue/install/aws/9cc6b97024-private.pem.key")
    (mosquitto:connect "A72KPY36W6QTP.iot.us-east-1.amazonaws.com" :port 8883)
    (loop
      :for i :from 0 :to 20
      :do (progn
            (mosquitto:process-some-events)
            (format t "Tick~%")
            (when (> i 8)
              (mosquitto:publish "topic/test" (babel:string-to-octets (princ-to-string (random 100)) :encoding :utf-8)))
            (sleep 1)))
    (format t "Done.")))
