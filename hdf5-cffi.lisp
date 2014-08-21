;;;; hdf5-cffi.lisp

(in-package #:hdf5-cffi)

(define-foreign-library hdf5
    (t (:default "libhdf5")))

(use-foreign-library hdf5)

;;; make sure the HDF5 library is initialized
(foreign-funcall "H5open")
