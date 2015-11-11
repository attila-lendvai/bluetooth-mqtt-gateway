(in-package :bt-mqtt-gateway)

(def function executable-toplevel ()
  (bind ((options (append (list +help-command-line-option+)
                          +generic-command-line-options+))
         (arguments (parse-command-line-arguments options))
         (swank-port 10000)
         (disable-debugger nil))
    (assert (not (null arguments)) (arguments) "Something is wrong: command line arguments are empty, but at least the default values should be there.")
    (process-help-command-line-argument options arguments)
    (restart-case
        (with-layered-error-handlers ((lambda (error)
                                        (print-error-safely (build-error-log-message :error-condition error
                                                                                     :message "Error reached toplevel in the main thread"))
                                        (unless disable-debugger
                                          (invoke-debugger error)))
                                      (lambda (&key &allow-other-keys)
                                        (print-error-safely "Calling QUIT from toplevel error handler")
                                        (quit 3)))
          (labels
              ((console (format &rest args)
                 (apply 'format t format args))
               (running-signal-handler (signal code scp)
                 (declare (ignore signal code scp))
                 ;;(production.info "SIGTERM/SIGINT was received, initiating shutdown")
                 (console "~%SIGTERM/SIGINT was received, initiating shutdown")
                 ;; TODO
                 (quit 2)
                 ))
            (sb-sys:enable-interrupt sb-unix:sigterm #'running-signal-handler)
            (sb-sys:enable-interrupt sb-unix:sigint #'running-signal-handler))
          (start-swank-server swank-port)
          ;; TODO
          (sleep 100000)
          +process-return-code/no-error+)
      (abort nil
        :report (lambda (stream)
                  (format stream "Give up starting the image and quit the VM process with exit code 2"))
        (quit 2)))))
