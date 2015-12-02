;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :bt-mqtt-gateway)

(defun print-le-advertising-event (buffer)
  (let ((props (parse-le-advertising-event buffer)))
    (print (getf props :mac-address))))

(defun parse-le-advertising-event (buffer)
  (c-let ((event hu.dwim.bluez.ffi:evt-le-meta-event :from (cffi:inc-pointer (ptr buffer) (+ 1 bluez:+hci-event-hdr-size+))))
    (when (eql (event :subevent)
               bluez:+evt-le-advertising-report+)
      ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr buffer) 45)))
      ;;(terpri)
      (c-let ((info bluez:le-advertising-info :from (cffi:inc-pointer (event :data &) 1)))
        (append (bluez:parse-extended-inquiry-response (info :data &) (info :length))
                (list :mac-address (bluez:bdaddr->string (info :bdaddr))))))))

(defun executable-toplevel/stage2 ()
  (mosquitto:lib-init :minimum-version '(1 4 4))
  (log.debug "Mosquitto lib version: ~A" (apply 'format nil "~D.~D.~D~%" (multiple-value-list (mosquitto:lib-version))))
  (main-loop)
  (format t "Exiting~%")
  +process-return-code/no-error+)

;; topic: $aws/things/AttilaGateway/shadow/update

(cffi:defcallback mosquitto/callback/log :void ((client :pointer) (userdata :pointer) (level :int) (message :string))
  (declare (ignore client userdata))
  (mosquitto.debug "Callback: ~D ~S" level message))

(def function hci/enable-le-scanning (socket)
  (c-fun/rc bluez:hci-le-set-scan-parameters socket 1 #x10 #x10 0 0 1000)
  (c-fun/rc bluez:hci-le-set-scan-enable socket 1 1 1000)
  (c-with (;;(original-filter bluez:hci-filter)
           ;;(original-filter-struct-size bluez:socklen-t :value (autowrap:sizeof 'bluez:hci-filter))
           (new-filter bluez:hci-filter))
    ;; (format t "backing up the filter, (original-filter-struct-size &) ~S~%" (original-filter-struct-size &))
    ;; (c-fun/rc bluez:getsockopt socket bluez:+sol-hci+ bluez:+hci-filter+ (original-filter &) (original-filter-struct-size &))
    ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr original-filter) (autowrap:sizeof 'bluez:hci-filter))))
    (bluez:hci-filter/initialize-for-le-scanning new-filter)
    ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr new-filter) (autowrap:sizeof 'bluez:hci-filter))))
    (c-fun/rc bluez:setsockopt socket bluez:+sol-hci+ bluez:+hci-filter+ (new-filter &) (autowrap:sizeof 'bluez:hci-filter)))
  (values))

(def class* bluetooth-peer ()
  ((mac-address)
   (name)
   (last-seen)))

(def print-object (bluetooth-peer :identity nil)
  (format t "mac ~A, name ~S" (mac-address-of -self-) (name-of -self-)))

(defun bluetooth-time ()
  (get-universal-time))

(def special-variable *bluetooth-peers*)

(defun bluetooth-peers/known-count ()
  (hash-table-count *bluetooth-peers*))

(defun event/bluetooth-peer-noticed (event)
  (bind ((mac-address (getf event :mac-address))
         (name (or (getf event 'bluez:+eir-name-complete+)
                   (getf event 'bluez:+eir-name-short+)
                   "(unknown)")))
    (assert mac-address)
    (aif (gethash mac-address *bluetooth-peers*)
         (setf (last-seen-of it) (bluetooth-time))
         (bind ((bluetooth-peer (make-instance 'bluetooth-peer
                                               :name name
                                               :mac-address mac-address
                                               :last-seen (bluetooth-time))))
           (scanning.info "Noticed a new peer: ~A; ~A known peers" bluetooth-peer (bluetooth-peers/known-count))
           #+nil
           (with-open-hci-socket (socket :remote-device mac-address)
             (hu.dwim.bluez.ffi:hci-le-create-conn ))
           (setf (gethash mac-address *bluetooth-peers*)
                 bluetooth-peer)))))

(def function map-registered-bluetooth-peers (visitor)
  (iter (for (nil bluetooth-peer) :in-hashtable *bluetooth-peers*)
        (collect (funcall visitor bluetooth-peer))))

(defun main-loop ()
  (bind ((*bluetooth-peers* (make-hash-table :test 'equal)))
    (flet ((tls-data-file (name)
             (pathname (concatenate 'string "/opt/bt-mqtt-gateway/aws-cert/" name))))
      (mosquitto:with-new-session (:client-id "Lisp MQTT"
                                              :log-callback (cffi:callback mosquitto/callback/log)
                                              :tls-certificate-authority (tls-data-file "rootCA.pem")
                                              :tls-client-certificate (tls-data-file "9cc6b97024-certificate.pem.crt")
                                              :tls-private-key (tls-data-file "9cc6b97024-private.pem.key"))
        (mosquitto:connect "A72KPY36W6QTP.iot.us-east-1.amazonaws.com" :port 8883)
        (with-hci-connection-context ()
          (bind ((hci-connection (open-hci-connection :local-device "00:1A:7D:DA:71:13"
                                                      ;; :local-device "hci2"
                                                      ;; :remote-device "00:07:80:2E:CB:43"
                                                      ))
                 ((:read-only-slots hci-device-id socket) hci-connection))
            (bluetooth.debug "Opened bluetooth ~A, LE capable? ~S, device name: ~S" hci-connection (bluez:hci/is-device-le-capable? hci-device-id) (bluez:hci-device-name hci-device-id))
            (bluez:hci/reset-device hci-device-id)
            (bluetooth.debug "Device was reset successfully")
            (hci/enable-le-scanning socket)
            (bluetooth.debug "Scanning enabled")
            (let ((buffer-size bluez:+hci-max-event-size+))
              (c-with ((buffer bluez:uint8-t :count buffer-size))
                (loop
                  :with last-info-log = 0
                  :for bytes-read = (bluez.ffi:read socket (buffer &) buffer-size)
                  :while (or (>= bytes-read 0)
                             (eql autowrap:errno bluez:+eintr+)
                             (eql autowrap:errno bluez:+ewouldblock+))
                  :do
                  (when (> (abs (- (get-universal-time)
                                   last-info-log))
                           5)
                    (setf last-info-log (get-universal-time))
                    (scanning.debug "~A devices registered" (bluetooth-peers/known-count))
                    (map-registered-bluetooth-peers 'print))
                  (mosquitto:process-some-events)
                  ;;(mosquitto:publish "topic/test" (babel:string-to-octets (princ-to-string (random 100)) :encoding :utf-8))
                  (cond
                    ((plusp bytes-read)
                     ;;(format t "~&Read ~S bytes " bytes-read)
                     (bind ((event (parse-le-advertising-event (buffer &))))
                       (event/bluetooth-peer-noticed event)))
                    ((eql autowrap:errno bluez:+ewouldblock+)
                     ;;(format t "tick~%")
                     (sleep 0.1))))))
            (format t "Body finished~%")))))))
