(in-package :bt-mqtt-gateway)

;; most of this is a duplicate of hu.dwim.util

(def (constant e) +generic-command-line-options+
  '(#+nil(("verbose" #\Space)
     :type boolean
     :optional #t
     :documentation "Try to provide more information about what's happening.")
    #+nil(("pid-file" #\Space)
     :type string
     :documentation "The PID file is created when the server starts. The file will be deleted when the server stops.")
    (("swank-port" #\Space)
     :type integer
     :initial-value 4005
     :documentation "The port is used to connect to the running server with SLIME.")
    (("disable-debugger" #\Space)
     :type boolean
     :optional #t
     :documentation "Disable the debugger, so that in case of unhandled toplevel errors the process quits. True by default unless in --repl mode.")
    #+nil(("repl" #\Space)
     :type boolean
     :optional #t
     :documentation "If provided then instead of starting the server only a REPL will be started. This might be useful for mainenance, testing and bug fixing.")))
