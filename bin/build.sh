#!/bin/sh
#| -*- mode: lisp; coding: utf-8-unix -*-

# this script assumes SBCL at multiple places. ideally it should use
# cl-launch, but see Fare's thoughts below (from around 2014):
#
# Quick answer: cl-launch could help indeed, though it might need some love.
#
# A few things cl-launch 4 will do for you:
# * abstract over implementation. More useful if you're on Windows or
# ARM, where SBCL support is not as good and you'll want CCL instead.
# * manage the painful loading of asdf and uiop and quicklisp — note
# that any version of ASDF good enough for cl-launch 4 will have UIOP.
# * let you easily specify --system hu.dwim.logger --system
# maru+hu.dwim.logger --system swank --file setup.lisp, etc.
# * (via UIOP) setup the debugger hooks, portably.
#
# Things it won't do (yet):
# * support a non-standard quicklisp installation rather than the
# builtin ~/quicklisp or ~/.quicklisp. It's a SMOP to add (see e.g. how
# the source-registry is handled), but still has to be done. I believe
# ${x#*=} is a standard enough shell construct that one could give an
# argument to --quicklisp with --quicklisp=foo.
# * same for output translation — though you can already export the
# environment variable.

. `dirname "$0"`/environment.sh

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

LISP=${SCRIPT_DIR}/../../sbcl/run-sbcl.sh
LISP=`readlink -f ${LISP}`

cd "${SCRIPT_DIR}"

echo "*** "`date`" Building '${DWIM_PROJECT_NAME}' from workspace '${DWIM_WORKSPACE}'"

BUILD_LOG_FILE="${DWIM_EXECUTABLE_CORE_FILE}.build-log"

export CL_SOURCE_REGISTRY="(:source-registry (:also-exclude \"sbcl\" \"disabled-systems\") \
                                             (:tree \"${DWIM_WORKSPACE}\") \
                                             (:tree \"/usr/local/share/common-lisp/source/\") \
                                             :ignore-inherited-configuration)"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${DWIM_WORKSPACE}\" (\"${DWIM_INSTALL_PATH}/.cache/common-lisp/\" :implementation)) :ignore-inherited-configuration)"

# i don't know how to convince program-op to overwrite the output, so delete from here...
# a suggested alternative: (defmethod asdf:perform :before ((op asdf:program-op) (sys (eql (asdf:find-system :my-system)))) (uiop:delete-file-if-exists (asdf:output-file op sys)))
# another one, better: (defmethod asdf/plan:traverse-action :before (plan (op asdf:program-op) (sys (eql (asdf:find-system :system))) niip) (uiop:delete-file-if-exists (asdf:output-file op sys)))
rm "${DWIM_EXECUTABLE_CORE_FILE}.new"

# "call" the lisp part below
exec ${LISP} --dynamic-space-size "${DWIM_MAXIMUM_MEMORY_SIZE}" --end-runtime-options --no-sysinit --no-userinit --script "$0" --end-toplevel-options 2>&1 | tee ${BUILD_LOG_FILE}

chown ${DWIM_DAEMON_USER}:${DWIM_DAEMON_USER} "${DWIM_EXECUTABLE_CORE_FILE}.new"
chmod o-rwx "${DWIM_EXECUTABLE_CORE_FILE}.new"

echo "*** "`date`" Finished building ${DWIM_PROJECT_NAME}, executable should be at ${DWIM_EXECUTABLE_CORE_FILE}.new"

# let's quit the shell part before the shell interpreter runs on the lisp stuff below
kill -INT $$

# and from here follows the lisp part that gets invoked by the above shell part |#

(in-package :cl-user)

(require :asdf)

(let ((output-translations (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
  (assert output-translations)
  (defun bt-mqtt-gateway/set-output-translations-hook ()
    (asdf:initialize-output-translations output-translations)))

;; WARNING: this is fragile as is, because the UIOP API doesn't state it clearly that hooks registered later will be called later, but
;; we need that behavior for shadowing the default UIOP behavior of setting the output-translations to the user's home.
(uiop:register-image-restore-hook 'bt-mqtt-gateway/set-output-translations-hook)

(defmethod asdf:output-files ((o asdf:program-op) (s (eql (asdf:find-system :bt-mqtt-gateway))))
  (let ((exe-path (uiop:getenv "DWIM_EXECUTABLE_CORE_FILE")))
    (if exe-path
        (values (list (concatenate 'string exe-path ".new")) t)
        (call-next-method))))

(defun make-all-loaded-asdf-systems-immutable ()
  (let ((loaded-systems/name (asdf:already-loaded-systems)))
    ;; (format t "~%Making the following ASDF systems immutable:~%~A~%~%" loaded-systems/name)
    (map nil 'asdf:register-immutable-system loaded-systems/name))
  (values))

(pushnew 'make-all-loaded-asdf-systems-immutable uiop:*image-dump-hook*)

;; first make sure everything is loaded, so that when the image is dumped then our hook above makes the required systems immutable.
;; ...except that it quietly doesn't work. maybe because of making :bt-mqtt-gateway asdf system immutable?
;;(asdf:load-system :bt-mqtt-gateway)
;;(setf asdf:*immutable-systems* (uiop:list-to-hash-set (asdf:already-loaded-systems)))

(asdf:operate 'asdf:program-op :bt-mqtt-gateway)

(format *error-output* "~%*** Something went wrong, SBCL should not return from asdf:program-op...~%~%")

;; this is dead man's land here on implementations like SBCL that can only SAVE-LISP-AND-DIE where the die part is not optional.
