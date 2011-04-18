;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/server.lisp,v 1.43 2008/04/09 08:17:48 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2011, Cyrus Harmon. All rights reserved.

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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun default-document-directory (&optional sub-directory)
    (asdf:system-relative-pathname :hunchentoot (format nil "www/~@[~A~]" sub-directory))))

(defclass dispatcher () 
  ((document-root :initarg :document-root
                  :accessor dispatcher-document-root
                  :documentation "Directory pathname that points to
files that are served by the acceptor if no more specific
acceptor-dispatch-request method handles the request."))
  (:default-initargs
   :document-root (load-time-value (default-document-directory))))

(defmethod dispatch-request ((dispatcher dispatcher) request)
  "Detault implementation of the request dispatch method, generates a +http-not-found+ error+."
  (declare (ignore request))
  (if (dispatcher-document-root dispatcher)
      (handle-static-file (merge-pathnames (if (equal (script-name*) "/")
                                               "index.html"
                                               (subseq (script-name*) 1))
                                           (dispatcher-document-root dispatcher)))
      (setf (return-code *reply*) +http-not-found+)))
