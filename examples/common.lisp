;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(defpackage #:h5ex
  (:documentation "hdf5-examples: Common helpers for hdf5-cffi examples.")
  (:use #:cl)
  (:export create-null-dataspace
           create-scalar-dataspace
           create-simple-dataspace
           create-c-string-type
           create-c-string-type-utf8
           create-f-string-type
           create-f-string-type-utf8))

(in-package :h5ex)

;; dataspaces

(defun create-null-dataspace ()
  "Create a null dataspace"
  (hdf5:h5screate :H5S-NULL))


(defun create-scalar-dataspace ()
  "Create a scalar dataspace"
  (hdf5:h5screate :H5S-SCALAR))


(defun create-simple-dataspace (dims &optional maxdims)
  "Create a simple dataspace"
  ;; list arguments expected
  (unless (and (consp dims) (or (null maxdims) (consp maxdims)))
    (error "List arguments expected."))
  ;; rank check
  (when (and maxdims
             (not (= (list-length dims) (list-length maxdims))))
    (error "Rank mismatch in simple dataspace definition."))
  
  (let* ((rank (list-length dims))
         (dims-ptr (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                       :initial-contents dims))
         (maxdims-ptr (if maxdims
                          (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                              :initial-contents maxdims)
                          hdf5:+NULL+))
         (space (hdf5:h5screate-simple rank dims-ptr maxdims-ptr)))
    (cffi:foreign-free dims-ptr)
    (when maxdims (cffi:foreign-free maxdims-ptr))
    space))

;; common string datatypes

(defun create-c-string-type (&optional length)
  "Create a C-style string datatype"
  (when length
    (unless (and (integerp length) (< 0 length))
      (error "Length must be a positive integer.")))
  
  (let ((result (hdf5:h5tcopy hdf5:+H5T-C-S1+)))
    (hdf5:h5tset-size result (if length length hdf5:+H5T-VARIABLE+))
    result))


(defun create-c-string-type-utf8 (&optional length)
  (let ((result (create-c-string-type length)))
    (hdf5:h5tset-cset result :H5T-CSET-UTF8)
    result))


(defun create-f-string-type (&optional length)
  "Create a FORTRAN-style string datatype"
  (when length
    (unless (and (integerp length) (< 0 length))
      (error "Length must be a positive integer.")))
  
  (let ((result (hdf5:h5tcopy hdf5:+H5T-FORTRAN-S1+)))
    (hdf5:h5tset-size result (if length length hdf5:+H5T-VARIABLE+))
    result))


(defun create-f-string-type-utf8 (&optional length)
  (let ((result (create-f-string-type length)))
    (hdf5:h5tset-cset result :H5T-CSET-UTF8)
    result))
