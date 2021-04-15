;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :bt-mqtt-gateway)

(def logger log ())

(def logger mosquitto (log))

(def logger bluetooth (log))

(def logger scanning (bluetooth))
