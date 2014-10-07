;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

(in-package #:hdf5)

(cffi:defcfun "H5Rcreate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-Create"
  (ref :pointer)
  (loc-id hid-t)
  (name :string)
  (ref-type H5R-type-t)
  (space-id hid-t))

(cffi:defcfun "H5Rdereference" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-Dereference"
  (dataset hid-t)
  (ref-type H5R-type-t)
  (ref :pointer))

(cffi:defcfun "H5Rget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetName"
  (loc-id hid-t)
  (ref-type H5R-type-t)
  (ref :pointer)
  (name (:pointer :char))
  (size size-t))

(cffi:defcfun "H5Rget_obj_type2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetObjType2"
  (loc-id hid-t)
  (ref-type H5R-type-t)
  (ref :pointer)
  (obj-type (:pointer H5O-type-t)))

(cffi:defcfun "H5Rget_region" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5R.html#Reference-GetRegion"
  (dataset hid-t)
  (ref-type H5R-type-t)
  (ref :pointer))
