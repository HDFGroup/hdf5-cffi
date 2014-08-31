;;;; h5l.lisp - See H5Lpublic.h .

(in-package #:hdf5-cffi)

(defconstant +H5L-MAX-LINK-NAME-LEN+ (1- (ash 1 32)))

(defcenum h5l-type-t
  (:H5L-TYPE-ERROR    -1)
  (:H5L-TYPE-HARD      0)
  (:H5L-TYPE-SOFT      1)
  (:H5L-TYPE-EXTERNAL  64)
  (:H5L-TYPE-MAX       255))

(defcunion h5l-info-union-t
  (address haddr-t)
  (val-size size-t))

(defcstruct h5l-info-t
  (type h5l-type-t)
  (corder-valid hbool-t)
  (corder :int64)
  (cset h5t-cset-t)
  (u (:union h5l-info-union-t)))

(defcfun "H5Lcreate_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateExternal"
  (target-file-name :string)
  (target-obj-name :string)
  (link-loc-id hid-t)
  (link-name :string)
  (lcpl-id hid-t)
  (lapl-id hid-t))

(defcfun "H5Lcreate_hard" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateHard"
  (obj-loc-id hid-t)
  (obj-name :string)
  (link-loc-id hid-t)
  (link-name :string)
  (lcpl-id hid-t)
  (lapl-id hid-t))

(defcfun "H5Lcreate_soft" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-CreateSoft"
  (target-path :string)
  (link-loc-id hid-t)
  (link-name :string)
  (lcpl-id hid-t)
  (lapl-id hid-t))

(defcfun "H5Ldelete" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Delete"
  (loc-id hid-t)
  (name :string)
  (lapl-id hid-t))

(defcfun "H5Lexists" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Exists"
  (loc-id hid-t)
  (name :string)
  (lapl-id hid-t))

(defcfun "H5Lget_info" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-GetInfo"
  (link-loc-id hid-t)
  (link-name :string)
  (link-buff (:pointer (:struct h5l-info-t)))
  (lapl-id hid-t))

(defcfun "H5Lvisit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-Visit"
  (group-id hid-t)
  (index-type h5-index-t)
  (order h5-iter-order-t)
  (op :pointer)
  (op-data :pointer))

(defcfun "H5Lvisit_by_name" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html#Link-VisitByName"
  (loc-id hid-t)
  (group-name :string)
  (index-type h5-index-t)
  (order h5-iter-order-t)
  (op :pointer)
  (op-data :pointer)
  (lapl-id hid-t))
