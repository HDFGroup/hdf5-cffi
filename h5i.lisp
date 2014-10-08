;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(in-package #:hdf5)

(cffi:defcfun "H5Iget_file_id" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetFileId"
  (obj-id hid-t))

(cffi:defcfun "H5Iget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetType"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(cffi:defcfun "H5Iget_type" H5I-type-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetName"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(cffi:defcfun "H5Iis_valid" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-IsValid"
  (obj-id hid-t))

(cffi:defcfun "H5Inmembers" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-NMembers"
  (type H5I-type-t)
  (num-members (:pointer hsize-t)))

(cffi:defcfun "H5Itype_exists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-TypeExists"
  (type H5I-type-t))
