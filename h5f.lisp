;;;; See H5Fpublic.h .

(in-package #:hdf5-cffi)

;;; flags for h5fcreate and h5fopen

(defbitfield h5f-intent-flags :unsigned-int
             (:rdonly #x0000)
             (:rdwr   #x0001)
             (:trunc  #x0002)
             (:excl   #x0004)
             (:debug  #x0008)
             (:creat  #x0010))

;;; enumeration for flush scope

(defcenum h5f-scope-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-Flush"
  (:H5F_SCOPE_LOCAL  0)
  (:H5F_SCOPE_GLOBAL 1))

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

(defcfun "H5Fget_filesize" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetFilesize"
  (file-id hid-t)
  (size (:pointer hsize-t)))

(defcfun "H5Fget_intent" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetIntent"
  (file-id hid-t)
  (intent (:pointer h5f-intent-flags)))

(defcfun "H5Fget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html#File-GetName"
  (obj-id hid-t)
  (name (:pointer :char))
  (size size-t))

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
