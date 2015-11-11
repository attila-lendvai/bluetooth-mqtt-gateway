;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :bt-mqtt-gateway
  (:use :autowrap.minimal
        :plus-c
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:export)
  (:shadowing-import-from :hu.dwim.bluez
                          #:c-fun/rc)
  (:shadowing-import-from :hu.dwim.mosquitto
                          #:c-fun/not-null)
  (:local-nicknames
   (#:bluez :hu.dwim.bluez)
   (#:bluez.ffi :hu.dwim.bluez.ffi)
   (#:mosquitto :hu.dwim.mosquitto))
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-readtime-wrapper-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)))
