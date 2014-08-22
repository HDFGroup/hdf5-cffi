;;;; hdf5-cffi.lisp

(in-package #:hdf5-cffi)

(define-foreign-library hdf5
    (t (:default "libhdf5")))

(use-foreign-library hdf5)

;;; make sure the HDF5 library is initialized
(let ((ierr (foreign-funcall "H5open")))
  (if (or (not ierr) (< ierr 0))
      (error "Can't open the HDF5 library.")))

;;; "sizing up the environment"

(defconstant +SIZE-OF-INT+ (foreign-type-size :int))
(defconstant +SIZE-OF-LONG+ (foreign-type-size :long))
(defconstant +SIZE-OF-LONG-LONG+ (foreign-type-size :long-long))
(defconstant +SIZE-OF-SIZE-T+ (foreign-type-size :pointer))
