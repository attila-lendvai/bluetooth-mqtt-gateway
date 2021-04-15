;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :bt-mqtt-gateway)

(defun print-le-advertising-event (buffer)
  (let ((props (parse-le-advertising-event buffer)))
    (print (getf props :mac-address))))

;; http://processors.wiki.ti.com/index.php/CC256x_VS_HCI_Commands
(defun parse-le-advertising-event (buffer)
  ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (ptr buffer) 45)))
  ;;(terpri)
  (assert (eql (cffi:mem-ref buffer :char) #,HCI_EVENT_PKT))
  (let ((event (cffi:inc-pointer buffer (+ 1 #,HCI_EVENT_HDR_SIZE))))
    (bind ((subevent-type (cffi:foreign-slot-value event '#,evt_le_meta_event '#,subevent)))
      (case subevent-type
        (#.#,EVT_LE_ADVERTISING_REPORT
         (let* (#+nil(info bluez:le-advertising-info :from (cffi:inc-pointer
                                                            (cffi:foreign-slot-pointer event bluez.ffi::evt_le_meta_event blueffi::data)
                                                            1))
                (info (cffi:inc-pointer
                       (cffi:foreign-slot-pointer event '#,evt_le_meta_event '#,data)
                       1))
                (data-pointer (cffi:foreign-slot-pointer info '#,le_advertising_info '#,data))
                (data-length (cffi:foreign-slot-value info '#,le_advertising_info '#,length)))
           (append (bluez:parse-extended-inquiry-response data-pointer data-length)
                   (list :mac-address (bluez:bdaddr->string (cffi:foreign-slot-value info '#,le_advertising_info '#,bdaddr)))
                   (list :rssi (cffi:mem-ref data-pointer :char data-length)))))
        (t
         (format t "Unknown subevent type: ~S" subevent-type))))))

(defun print-event-info (buffer buffer-size)
  (print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector buffer buffer-size)))
  (let ((event-header (cffi:inc-pointer buffer 1)))
    (format t "Event type: ~A" (cffi:foreign-slot-value event-header '#,hci_event_hdr '#,evt))))

(defun executable-toplevel/stage2 ()
  (unwind-protect
       (progn
         (mosquitto:lib-init :minimum-version '(1 4 4))
         (log.debug "Mosquitto lib version: ~A" (apply 'format nil "~D.~D.~D~%" (multiple-value-list (mosquitto:lib-version))))
         (main-loop))
    (mosquitto:lib-cleanup))
  (format t "Exiting~%")
  +process-return-code/no-error+)

;; topic: $aws/things/AttilaGateway/shadow/update

(cffi:defcallback mosquitto/callback/log :void ((client :pointer) (userdata :pointer) (level :int) (message :string))
  (declare (ignore client userdata))
  (mosquitto.debug "Callback: ~D ~S" level message))

(def function hci/enable-le-scanning (socket &key all-events?)
  (#,hci_le_set_scan_parameters socket 1 #x10 #x10 0 0 1000)
  (cffi:with-foreign-objects (;;(original-filter bluez:hci-filter)
                              ;;(original-filter-struct-size bluez:socklen-t :value (autowrap:sizeof 'bluez:hci-filter))
                              (new-filter '(:struct #,hci_filter)))
    ;; (format t "backing up the filter, (original-filter-struct-size &) ~S~%" (original-filter-struct-size &))
    ;; (c-fun/rc bluez:getsockopt socket bluez:+sol-hci+ bluez:+hci-filter+ (original-filter &) (original-filter-struct-size &))
    ;;(print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector (autowrap:ptr original-filter) (autowrap:sizeof 'bluez:hci-filter))))
    (when all-events?
      ;; KLUDGE it's not ok this way, rename, refactor, etc
      (let ((event-mask-pointer (cffi:foreign-slot-pointer new-filter '(:struct #,hci_filter) '#,event_mask)))
        (setf (cffi:mem-ref event-mask-pointer '#,uint32_t 0) #xffffffff)
        (setf (cffi:mem-ref event-mask-pointer '#,uint32_t 1) #xffffffff)))
    (bluez:hci-filter/initialize-for-le-scanning new-filter)
    ;; good: "10 00 00 00 00 00 00 00 00 00 00 40 00 00 00 00"
    ;; (print (ironclad:byte-array-to-hex-string (bluez::copy-sap-to-byte-vector new-filter (cffi:foreign-type-size '(:struct bluez:hci_filter)))))
    (c-fun/rc #,setsockopt socket #,SOL_HCI #,HCI_FILTER new-filter (cffi:foreign-type-size '(:struct #,hci_filter))))
  (#,hci_le_set_scan_enable socket 1 0 1000)
  (values))

(def class* bluetooth-peer ()
  ((mac-address)
   (name)
   (last-seen)
   (rssi)))

(def print-object (bluetooth-peer :identity nil)
    (format t "mac: ~A, name: ~S, RSSI: ~A (~A sec)"
            (mac-address-of -self-)
            (name-of -self-)
            (rssi-of -self-)
            (- (bluetooth-time) (last-seen-of -self-))))

(defun bluetooth-time ()
  (get-universal-time))

(def special-variable *bluetooth-peers*)

(defun bluetooth-peers/known-count ()
  (hash-table-count *bluetooth-peers*))

(defun event/bluetooth-peer-noticed (socket event)
  (bind ((peer-address (getf event :mac-address))
         (name (or (getf event 'bluez:+eir-name-complete+)
                   (getf event 'bluez:+eir-name-short+)
                   "(unknown)")))
    (assert peer-address)
    (flet ((update (bluetooth-peer)
             (setf (last-seen-of bluetooth-peer) (bluetooth-time))
             (setf (rssi-of bluetooth-peer) (getf event :rssi))))
      (aif (gethash peer-address *bluetooth-peers*)
           (update it)
           (bind ((bluetooth-peer (make-instance 'bluetooth-peer
                                                 :name name
                                                 :mac-address peer-address)))
             (update bluetooth-peer)
             (scanning.info "Noticed a new peer: ~A; ~A known peers" bluetooth-peer (bluetooth-peers/known-count))
             ;; hci-le-create-conn timeouts: https://stackoverflow.com/questions/24945620/excessive-bluetooth-le-timeouts-on-linux
             (gather-bluetooth-peer-info socket peer-address)
             (setf (gethash peer-address *bluetooth-peers*)
                   bluetooth-peer))))))

(def function gather-bluetooth-peer-info (socket peer-address)
  ;; scan+connect ugy tunik nem megy. ez csak onmagaban mukodik, reset utan.
  #+nil
  (with-hci-socket-context ()
    (bind ((hci-socket (open-hci-socket :adapter "hci0")))
      (GATHER-BLUETOOTH-PEER-INFO (fd-of hci-socket) "00:07:80:2E:B7:64")))
  (cffi:with-foreign-objects ((peer-bdaddr '#,bdaddr_t)
                              (handle '#,uint16_t))
    (bluez:string->bdaddr peer-address peer-bdaddr)
    (c-fun/rc #,hci_le_create_conn
              socket 4 4 0 #,LE_PUBLIC_ADDRESS peer-bdaddr
              #,LE_PUBLIC_ADDRESS #xf #xf 0 1000 0 0 handle 25000)))

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
        (with-hci-socket-context ()
          (bind ((hci-socket (open-hci-socket
                              ;; :adapter "00:1A:7D:DA:71:13" ;; smart enabled
                              ;; :adapter "00:1A:7D:DA:71:10" ;; smart enabled
                              :adapter "hci2"
                              ;; :adapter "5C:F3:70:6A:0E:7F"
                              ;; :peer-address "00:07:80:2E:CB:43"
                              ))
                 (socket (fd-of hci-socket))
                 (hci-adapter-id (hci-adapter-id-of hci-socket)))
            (bluetooth.debug "Opened bluetooth ~A, LE capable? ~S, device name: ~S" hci-socket (bluez:hci/is-device-le-capable? hci-adapter-id) (bluez:hci/adapter-name hci-adapter-id))
            (bluez:hci/reset-adapter hci-adapter-id)
            (bluetooth.debug "Device was reset successfully")
            (hci/enable-le-scanning socket)
            (bluetooth.debug "Scanning enabled")
            (let ((buffer-size #,HCI_MAX_EVENT_SIZE))
              (cffi:with-foreign-object (buffer '#,uint8_t buffer-size)
                (loop
                  :with last-info-log = 0
                  :for bytes-read = (#,read socket buffer buffer-size)
                  :while (or (>= bytes-read 0)
                             (eql *errno* #,EINTR)
                             (eql *errno* #,EWOULDBLOCK))
                  :do
                  (when (> (abs (- (get-universal-time)
                                   last-info-log))
                           5)
                    ;; do this also only every 5 secs
                    (when (not (zerop last-info-log))
                      ;; TODO KLUDGE to avoid calling this before we get a CONNACK...
                      (mosquitto:publish "topic/test" (babel:string-to-octets (princ-to-string (random 100)) :encoding :utf-8)))
                    (setf last-info-log (get-universal-time))
                    (scanning.debug "~A devices registered" (bluetooth-peers/known-count))
                    (map-registered-bluetooth-peers 'print))
                  (mosquitto:process-some-events)
                  (cond
                    ((plusp bytes-read)
                     ;;(format t "~&Read ~S bytes " bytes-read)
                     ;;(print-event-info (buffer &) bytes-read)
                     (bind ((event (parse-le-advertising-event buffer)))
                       (event/bluetooth-peer-noticed socket event)))
                    ((eql *errno* #,EWOULDBLOCK)
                     ;;(format t "tick~%")
                     (sleep 0.1))))))
            (format t "Body finished~%")))))))
