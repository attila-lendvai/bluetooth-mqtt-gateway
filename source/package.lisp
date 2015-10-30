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
        :hu.dwim.syntax-sugar)
  (:export)
  (:shadowing-import-from :hu.dwim.bluez
                          #:c-fun/rc)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-readtime-wrapper-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)))

(in-package :bt-mqtt-gateway)

(defun ensure-package-nickname (package nickname)
  (let ((nickname (string nickname))
        (nicknames (package-nicknames package)))
    (unless (member nickname nicknames :test 'equal)
      (pushnew nickname nicknames)
      (rename-package package package nicknames))
    nicknames))

(ensure-package-nickname :hu.dwim.bluez :bluez)
(ensure-package-nickname :hu.dwim.bluez.ffi :bluez.ffi)
