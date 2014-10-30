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

(cffi:defcunion _u-t
  (address  haddr-t)
  (val-size size-t))

(cffi:defcstruct h5l-info-t "H5L_info_t"
  (type         h5l-type-t)
  (corder-valid hbool-t)
  (corder       :int64)
  (cset         h5t-cset-t)
  (u            (:union _u-t)))

(cffi:defcfun "H5Lcreate_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateExternal"
  (target-file-name :string)
  (target-obj-name  :string)
  (link-loc-id      hid-t)
  (link-name        :string)
  (lcpl-id          hid-t)
  (lapl-id          hid-t))

(cffi:defcfun "H5Lcreate_hard" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateHard"
  (obj-loc-id  hid-t)
  (obj-name    :string)
  (link-loc-id hid-t)
  (link-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(cffi:defcfun "H5Lcreate_soft" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateSoft"
  (target-path :string)
  (link-loc-id hid-t)
  (link-name   :string)
  (lcpl-id     hid-t)
  (lapl-id     hid-t))

(cffi:defcfun "H5Ldelete" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Delete"
  (loc-id  hid-t)
  (name    :string)
  (lapl-id hid-t))

(cffi:defcfun "H5Lexists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Exists"
  (loc-id  hid-t)
  (name    :string)
  (lapl-id hid-t))

(cffi:defcfun "H5Lget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetInfo"
  (link-loc-id hid-t)
  (link-name   :string)
  (link-buff   (:pointer (:struct h5l-info-t)))
  (lapl-id     hid-t))

(cffi:defcfun "H5Lget_name_by_idx" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetNameByIdx"
  (link-loc-id hid-t)
  (group-name  :string)
  (index-field h5-index-t)
  (order       h5-iter-order-t)
  (n           hsize-t)
  (name        (:pointer :char))
  (size        size-t)
  (lapl-id     hid-t))

(cffi:defcfun "H5Literate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Iterate"
  (group-id   hid-t)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (idx        (:pointer hsize-t))
  (op         :pointer)
  (op-data    :pointer))

(cffi:defcfun "H5Lvisit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Visit"
  (group-id   hid-t)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (op         :pointer)
  (op-data    :pointer))

(cffi:defcfun "H5Lvisit_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-VisitByName"
  (loc-id     hid-t)
  (group-name :string)
  (index-type h5-index-t)
  (order      h5-iter-order-t)
  (op         :pointer)
  (op-data    :pointer)
  (lapl-id    hid-t))
