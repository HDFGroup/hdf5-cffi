;;;; h5a.lisp - See H5Apublic.h .

(in-package #:hdf5-cffi)

(defcstruct h5a-info-t
  (corder_valid hbool-t)
  (corder h5o-msg-crt-idx-t)
  (cset h5t-cset-t)
  (data-size hsize-t))

(defcfun "H5Aclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Close"
  (attr-id hid-t))

(defcfun "H5Acreate1" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Create1"
  (loc-id hid-t)
  (attr-name :string)
  (type-id hid-t)
  (space-id hid-t)
  (acpl-id hid-t))

(defcfun "H5Acreate2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Create2"
  (loc-id hid-t)
  (attr-name :string)
  (type-id hid-t)
  (space-id hid-t)
  (acpl-id hid-t)
  (aapl-id hid-t))

(defcfun "H5Acreate_by_name" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-CreateByName"
  (loc-id hid-t)
  (obj-name :string)
  (attr-name :string)
  (type-id hid-t)
  (space-id hid-t)
  (acpl-id hid-t)
  (aapl-id hid-t)
  (lapl-id hid-t))

(defcfun "H5Adelete" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Delete"
  (loc-id hid-t)
  (attr-name :string))

(defcfun "H5Adelete_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-DeleteByName"
  (loc-id hid-t)
  (obj-name :string)
  (attr-name :string)
  (lapl-id hid-t))

(defcfun "H5Aexists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Exists"
  (obj-id hid-t)
  (attr-name :string))

(defcfun "H5Aexists_by_name" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-ExistsByName"
  (obj-id hid-t)
  (obj-name :string)
  (attr-name :string)
  (lapl-id hid-t))

(defcfun "H5Aget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetCreatePlist"
  (attr-id hid-t))

(defcfun "H5Aget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetInfo"
  (attr-id hid-t)
  (info (:pointer h5a-info-t)))

(defcfun "H5Aget_info_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetInfoByName"
  (loc-id hid-t)
  (obj-name :string)
  (attr-name :string)
  (info (:pointer h5a-info-t))
  (lapl-id hid-t))

(defcfun "H5Aget_name" ssize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetName"
  (attr-id hid-t)
  (buf-size size-t)
  (buf (:pointer :char)))

(defcfun "H5Aget_space" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetSpace"
  (attr-id hid-t))

(defcfun "H5Aget_storage_size" hsize-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetStorageSize"
  (attr-id hid-t))

(defcfun "H5Aget_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-GetType"
  (attr-id hid-t))

(defcfun "H5Aiterate2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Iterate2"
  (obj-id hid-t)
  (idx-type h5-index-t)
  (order h5-iter-order-t)
  (n (:pointer hsize-t))
  (op :pointer)
  (op-data :pointer))

(defcallback H5A-operator2-t herr-t
   ((location-id hid-t)
    (attr-name :string)
    (ainfo (:pointer h5a-info-t))
    (op-data :pointer))
  (progn
    (format t attr-name)
    0))

(defcfun "H5Aopen" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Open"
  (obj-id hid-t)
  (attr-name :string)
  (aapl-id hid-t))

(defcfun "H5Aopen_by_name" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-OpenByName"
  (loc-id hid-t)
  (obj-name :string)
  (attr-name :string)
  (aapl-id hid-t)
  (lapl-id hid-t))

(defcfun "H5Aread" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Read"
  (attr-id hid-t)
  (mem-type-id hid-t)
  (buf :pointer))

(defcfun "H5Arename" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Rename"
  (loc-id hid-t)
  (old-attr-name :string)
  (new-attr-name :string))

(defcfun "H5Arename-by-name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-RenameByName"
  (loc-id hid-t)
  (obj-name :string)
  (old-attr-name :string)
  (new-attr-name :string)
  (lapl-id hid-t))

(defcfun "H5Awrite" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5A.html#Annot-Write"
  (attr-id hid-t)
  (mem-type-id hid-t)
  (buf :pointer))
