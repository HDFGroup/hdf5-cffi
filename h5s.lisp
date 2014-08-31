;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; See H5Spublic.h .

(in-package #:hdf5-cffi)

(defconstant +H5S-ALL+ 0)

(defconstant +H5S-UNLIMITED+ (1- (ash 1 (* 8 +SIZE-OF-HSIZE-T+))))

(defconstant +H5S-MAX-RANK+ 32)

(defcenum h5s-class-t
  (:H5S-NO-CLASS -1)
  :H5S-SCALAR
  :H5S-SIMPLE
  :H5S-NULL)

(defcenum h5s-seloper-t
  (:H5S-SELECT-NOOP -1)
  :H5S-SELECT-SET
  :H5S-SELECT-OR
  :H5S-SELECT-AND
  :H5S-SELECT-XOR
  :H5S-SELECT-NOTB
  :H5S-SELECT-NOTA
  :H5S-SELECT-APPEND
  :H5S-SELECT-PREPEND
  :H5S-SELECT-INVALID)

(defcenum h5s-sel-type
  (:H5S-SEL-ERROR -1)
  :H5S-SEL-NONE
  :H5S-SEL-POINTS
  :H5S-SEL-HYPERSLABS
  :H5S-SEL-ALL
  :H5S-SEL-N)

(defcfun "H5Sclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Close"
  (space-id hid-t))

(defcfun "H5Scopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Copy"
  (space-id hid-t))

(defcfun "H5Screate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-Create"
  (types h5s-class-t))

(defcfun "H5Screate_simple" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-CreateSimple"
  (rank :int)
  (current-dims (:pointer hsize-t))
  (maximum-dims (:pointer hsize-t)))

(defcfun "H5Sget_select_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectBounds"
  (space-id hid-t)
  (start (:pointer hsize-t))
  (end (:pointer hsize-t)))

(defcfun "H5Sget_select_npoints" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectNpoints"
  (space-id hid-t))

(defcfun "H5Sget_select_type" h5s-sel-type
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-GetSelectType"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_dims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentDims"
  (space-id hid-t)
  (dims (:pointer hsize-t))
  (maxdims (:pointer hsize-t)))

(defcfun "H5Sget_simple_extent_ndims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentNdims"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_npoints" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentNpoints"
  (space-id hid-t))

(defcfun "H5Sget_simple_extent_type" h5s-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-ExtentType"
  (space-id hid-t))

(defcfun "H5Sis_simple" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-IsSimple"
  (space-id hid-t))

(defcfun "H5Sselect_hyperslab" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectHyperslab"
  (space-id hid-t)
  (select-operation h5s-seloper-t)
  (start (:pointer hsize-t))
  (stride (:pointer hsize-t))
  (count (:pointer hsize-t))
  (block (:pointer hsize-t)))

(defcfun "H5Sselect_valid" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html#Dataspace-SelectValid"
  (space-id hid-t))
