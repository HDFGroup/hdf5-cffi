;;;; hdf5-cffi.lisp

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
