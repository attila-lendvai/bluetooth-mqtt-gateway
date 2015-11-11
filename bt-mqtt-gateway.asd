(defsystem :bt-mqtt-gateway
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :entry-point "bt-mqtt-gateway::executable-toplevel"
  :build-pathname "bt-mqtt-gateway" ;; the base name of the executable
  :depends-on (:hu.dwim.bluez
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.mosquitto
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :hu.dwim.util/command-line
               :hu.dwim.util/error-handling+swank)
  :components ((:module "source"
                :components ((:file "duplicates" :depends-on ("package"))
                             (:file "main" :depends-on ("package" "duplicates"))
                             (:file "package")
                             (:file "server" :depends-on ("main"))))))
