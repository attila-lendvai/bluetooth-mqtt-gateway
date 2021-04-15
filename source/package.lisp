;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :bt-mqtt-gateway
  (:use :alexandria
        :hu.dwim.bluez/fancy
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:shadow #:log)
  (:export)
  (:shadowing-import-from :hu.dwim.bluez
                          #:c-fun/rc
                          #:*errno*)
  (:local-nicknames
   (#:bluez :hu.dwim.bluez)
   (#:bluez.ffi :hu.dwim.bluez.ffi)
   (#:mosquitto :hu.dwim.mosquitto))
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-readtime-wrapper-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)
   (hu.dwim.syntax-sugar:enable-case-preserving-syntax :packages '(:hu.dwim.bluez.ffi #+nil :hu.dwim.mosquitto.ffi))))
