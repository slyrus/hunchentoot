;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header$

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :hunchentoot)

(defclass ssl-socket-connector (socket-connector)
  ((ssl-certificate-file :initarg :ssl-certificate-file
                         :reader socket-connector-ssl-certificate-file
                         :documentation "A pathname designator for a
certificate file in PEM format.")
   (ssl-privatekey-file :initarg :ssl-privatekey-file
                        :reader socket-connector-ssl-privatekey-file
                        :documentation "A pathname designator for a
private key file in PEM format, or \(only on LispWorks) NIL if the
certificate file contains the private key.")
   (ssl-privatekey-password :initform nil
                            :initarg :ssl-privatekey-password
                            :reader socket-connector-ssl-privatekey-password
                            :documentation "The password for the
private key file or NIL for no password."))
  (:documentation "Create an SSL-SOCKET-CONNECTOR and pass this as
the :socket-connector to ACCEPTOR if you want an https server.  There
are two required initargs, :SSL-CERTIFICATE-FILE
and :SSL-PRIVATEKEY-FILE, for pathname designators denoting the
certificate file and the key file in PEM format.  On LispWorks, you
can have both in one file in which case the second initarg is
optional.  You can also use the
:SSL-PRIVATEKEY-PASSWORD initarg to provide a password \(as a string)
for the key file \(or NIL, the default, for no password)."))

(defmethod socket-connector-ssl-p ((socket-connector ssl-socket-connector))
  t)

;; usocket implementation

#-:lispworks
(defmethod initialize-instance :after ((socket-connector ssl-socket-connector)
                                       &rest initargs)
  (declare (ignore initargs))
  ;; LispWorks can read both from the same file, so we can default one
  #+:lispworks
  (unless (slot-boundp socket-connector 'ssl-privatekey-file)
    (setf (slot-value socket-connector 'ssl-privatekey-file)
          (socket-connector-ssl-certificate-file socket-connector)))
  ;; OpenSSL doesn't know much about Lisp pathnames...
  (setf (slot-value socket-connector 'ssl-privatekey-file)
        (namestring (truename
                     (socket-connector-ssl-privatekey-file socket-connector)))
        (slot-value socket-connector 'ssl-certificate-file)
        (namestring (truename
                     (socket-connector-ssl-certificate-file socket-connector)))))

#-:lispworks
(defmethod make-socket-connection ((socket-connector ssl-socket-connector) socket)
  ;; attach SSL to the stream if necessary
  (cl+ssl:make-ssl-server-stream (call-next-method)
                                 :certificate (socket-connector-ssl-certificate-file
                                               socket-connector)
                                 :key (socket-connector-ssl-privatekey-file
                                       socket-connector)
                                 :password (socket-connector-ssl-privatekey-password
                                            socket-connector)))

#+:lispworks
(defun make-ssl-server-stream (socket-stream &key certificate-file privatekey-file privatekey-password)
  "Given the socket stream SOCKET-STREAM attaches SSL to the
stream using the certificate file CERTIFICATE-FILE and the private key
file PRIVATEKEY-FILE.  Both of these values must be namestrings
denoting the location of the files and will be fed directly to
OpenSSL.  If PRIVATEKEY-PASSWORD is not NIL then it should be the
password for the private key file \(if necessary).  Returns the
stream."
  (flet ((ctx-configure-callback (ctx)
           (when privatekey-password
             (comm:set-ssl-ctx-password-callback ctx :password privatekey-password))
           (comm:ssl-ctx-use-certificate-file ctx
                                              certificate-file
                                              comm:ssl_filetype_pem)
           (comm:ssl-ctx-use-privatekey-file ctx
                                             privatekey-file
                                             comm:ssl_filetype_pem)))
    (comm:attach-ssl socket-stream
                     :ctx-configure-callback #'ctx-configure-callback)
    socket-stream))

#+:lispworks
(defmethod make-socket-connection ((socket-connector ssl-socket-connector) stream)
  ;; attach SSL to the stream if necessary
  (make-ssl-server-stream (call-next-method)
                          :certificate-file (socket-connector-ssl-certificate-file
                                             socket-connector)
                          :privatekey-file (socket-connector-ssl-privatekey-file
                                            socket-connector)
                          :privatekey-password (socket-connector-ssl-privatekey-password
                                                socket-connector)))

