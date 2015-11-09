(defsystem :bt-mqtt-gateway
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :entry-point "bt-mqtt-gateway::executable-toplevel"
  :build-pathname "bt-mqtt-gateway" ;; the base name of the executable
  :depends-on (:iolib
               :hu.dwim.bluez
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.mosquitto
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "main" :depends-on ("package"))
                             (:file "package")))))
