;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

(in-package #:hdf5-cffi)

(define-foreign-library hdf5
    (t (:default "libhdf5")))

(use-foreign-library hdf5)

;;; "sizing up the environment"

(defconstant +SIZE-OF-INT+ (foreign-type-size :int))
(defconstant +SIZE-OF-LONG+ (foreign-type-size :long))
(defconstant +SIZE-OF-LONG-LONG+ (foreign-type-size :long-long))
(defconstant +SIZE-OF-SIZE-T+ (foreign-type-size :pointer))

;;; time's a mystery

(defctype time-t :long)

;;; off_t is another one

(defctype off-t :long)
