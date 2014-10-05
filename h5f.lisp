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

(cffi:defcstruct sohm-t
  (hdr-size hsize-t)
  (msgs-info (:struct H5-ih-info-t)))

(cffi:defcstruct H5F-info-t
  (super-ext-size hsize-t)
  (sohm (:struct sohm-t)))

(cffi:defcfun "H5Fclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Close"
  (file-id hid-t))

(cffi:defcfun "H5Fcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Create"
  (filename :string)
  (flags :unsigned-int)
  (fcpl-id hid-t)
  (fapl-id hid-t))

(cffi:defcfun "H5Fflush" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Flush"
  (object-id hid-t)
  (scope H5F-scope-t))

(cffi:defcfun "H5Fget_access_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetAccessPlist"
  (file-id hid-t))

(cffi:defcfun "H5Fget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetCreatePlist"
  (file-id hid-t))

(cffi:defcfun "H5Fget_file_image" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFileImage"
  (file-id hid-t)
  (buf-ptr :pointer)
  (buf-len (:pointer size-t)))

(cffi:defcfun "H5Fget_filesize" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFilesize"
  (file-id hid-t)
  (size (:pointer hsize-t)))

(cffi:defcfun "H5Fget_freespace" hssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFreespace"
  (file-id hid-t))

(cffi:defcfun "H5Fget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetInfo"
  (obj-id hid-t)
  (file-info (:pointer (:struct H5F-info-t))))

(cffi:defcfun "H5Fget_intent" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetIntent"
  (file-id hid-t)
  (intent (:pointer :unsigned-int)))

(cffi:defcfun "H5Fget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetName"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

(cffi:defcfun "H5Fget_obj_count" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetObjCount"
  (file-id hid-t)
  (types :unsigned-int))

(cffi:defcfun "H5Fget_obj_ids" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetObjIDs"
  (file-id hid-t)
  (types :unsigned-int)
  (max-objs size-t)
  (obj-id-list (:pointer hid-t)))

(cffi:defcfun "H5Fis_hdf5" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-IsHDF5"
  (name :string))

(cffi:defcfun "H5Fmount" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Mount"
  (loc-id hid-t)
  (name :string)
  (child-id hid-t)
  (fmpl-id hid-t))

(cffi:defcfun "H5Fopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Open"
  (name :string)
  (flags :unsigned-int)
  (fapl-id hid-t))

(cffi:defcfun "H5Freopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Reopen"
  (file-id hid-t))

(cffi:defcfun "H5Funmount" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Unmount"
  (loc-id hid-t)
  (name :string))
