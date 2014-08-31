;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; See H5Fpublic.h .

(in-package #:hdf5-cffi)

(defcenum h5f-libver-t
  :H5F-LIBVER-EARLIEST
  :H5F-LIBVER-LATEST)

;;; flags for h5fcreate and h5fopen

(defbitfield h5f-intent-flags :unsigned-int
             (:rdonly #x0000)
             (:rdwr   #x0001)
             (:trunc  #x0002)
             (:excl   #x0004)
             (:debug  #x0008)
             (:creat  #x0010))

;;; flags for h5fget-obj-count and h5fget-obj-ids

(defbitfield h5f-obj-flags :unsigned-int
             (:file #x0001)
             (:dataset   #x0002)
             (:group     #x0004)
             (:datatype  #x0008)
             (:attribute #x0010)
             (:all       #x001F)
	     (:local     #x0020))

;;; enumeration for flush scope

(defcenum h5f-scope-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Flush"
  (:H5F-SCOPE-LOCAL  0)
  (:H5F-SCOPE-GLOBAL 1))

(defcenum h5f-close-degree-t
    "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFcloseDegree"
  (:H5F-CLOSE-DEFAULT 0)
  :H5F-CLOSE-WEAK
  :H5F-CLOSE-SEMI
  :H5F-CLOSE-STRONG)

(defcstruct h5f-sohm-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetInfo"
  (hdr-size hsize-t)
  (msgs-info (:struct h5-ih-info-t)))

(defcstruct h5f-info-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetInfo"
  (super-ext-size hsize-t)
  (sohm (:struct h5f-sohm-t)))

;;; functions

(defcfun "H5Fclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Close"
  (file-id hid-t))

(defcfun "H5Fcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Create"
  (filename :string)
  (flags h5f-intent-flags)
  (fcpl-id hid-t)
  (fapl-id hid-t))

(defcfun "H5Fflush" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Flush"
  (object-id hid-t)
  (scope h5f-scope-t))

(defcfun "H5Fget_access_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetAccessPlist"
  (file-id hid-t))

(defcfun "H5Fget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetCreatePlist"
  (file-id hid-t))

(defcfun "H5Fget_file_image" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFileImage"
  (file-id hid-t)
  (buf-ptr :pointer)
  (buf-len (:pointer size-t)))

(defcfun "H5Fget_filesize" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFilesize"
  (file-id hid-t)
  (size (:pointer hsize-t)))

(defcfun "H5Fget_freespace" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFreespace"
  (file-id hid-t))

(defcfun "H5Fget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetInfo"
  (obj-id hid-t)
  (file-info (:pointer (:struct h5f-info-t))))

(defcfun "H5Fget_intent" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetIntent"
  (file-id hid-t)
  (intent (:pointer h5f-intent-flags)))

(defcfun "H5Fget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetName"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(defcfun "H5Fget_obj_count" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetObjCount"
  (file-id hid-t)
  (types h5f-obj-flags))

(defcfun "H5Fget_obj_ids" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetObjIDs"
  (file-id hid-t)
  (types h5f-obj-flags)
  (max-objs size-t)
  (obj-id-list (:pointer hid-t)))

(defcfun "H5Fis_hdf5" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-IsHDF5"
  (name :string))

(defcfun "H5Fmount" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Mount"
  (loc-id hid-t)
  (name :string)
  (child-id hid-t)
  (fmpl-id hid-t))

(defcfun "H5Fopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Open"
  (name :string)
  (flags h5f-intent-flags)
  (fapl-id hid-t))

(defcfun "H5Freopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Reopen"
  (file-id hid-t))

(defcfun "H5Funmount" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Unmount"
  (loc-id hid-t)
  (name :string))
