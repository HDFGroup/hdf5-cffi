;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; See H5Ipublic.h .

(in-package #:hdf5-cffi)

;;; This will change in HDF5 1.10. Switch to 64-bit handles...

(defctype hid-t :int)

(defconstant +H5I-INVALID-HID+ -1)

(defcenum h5i-type-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetType"
  (:H5I_UNINIT  -2)
  (:H5I_BAD_ID  -1)
  (:H5I_FILE  1)
  :H5I_GROUP
  :H5I_DATATYPE
  :H5I_DATASPACE
  :H5I_DATASET
  :H5I_ATTR
  :H5I_REFERENCE
  :H5I_VFL
  :H5I_GENPROP_CLS
  :H5I_GENPROP_LST
  :H5I_ERROR_CLS
  :H5I_ERROR_MSG
  :H5I_ERROR_STACK
  :H5I_NTYPES)

;;; functions

(defcfun "H5Iget_file_id" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetFileId"
  (obj-id hid-t))

(defcfun "H5Iget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetType"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(defcfun "H5Iget_type" h5i-type-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-GetName"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(defcfun "H5Iis_valid" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-IsValid"
  (obj-id hid-t))

(defcfun "H5Inmembers" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-NMembers"
  (type h5i-type-t)
  (num-members (:pointer hsize-t)))

(defcfun "H5Itype_exists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5I.html#Identify-TypeExists"
  (type h5i-type-t))
