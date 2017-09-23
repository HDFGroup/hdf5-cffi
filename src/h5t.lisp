;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
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

(cffi:defcfun "H5Tarray_create2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-ArrayCreate2"
  (base-type-id hid-t)
  (rank         :uint)
  (dims         (:pointer hsize-t)))

(cffi:defcfun "H5Tclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Close"
  (dtype-id hid-t))

(cffi:defcfun "H5Tcommit2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Commit"
  (loc-id   hid-t)
  (name     :string)
  (dtype-id hid-t)
  (lcpl-id  hid-t)
  (tcpl-id  hid-t)
  (tapl-id  hid-t))

(cffi:defcfun "H5Tcommit_anon" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-CommitAnon"
  (loc-id   hid-t)
  (dtype-id hid-t)
  (tcpl-id  hid-t)
  (tapl-id  hid-t))

(cffi:defcfun "H5Tcommitted" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Committed"
  (dtype-id hid-t))

(cffi:defcfun "H5Tcompiler_conv" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-CompilerConv"
  (src-id hid-t)
  (dst-id hid-t))

(cffi:defcfun "H5Tconvert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Convert"
  (src-type   hid-t)
  (dest-type  hid-t)
  (nelmts     size-t)
  (buf        :pointer)
  (background :pointer)
  (plist      hid-t))

(cffi:defcfun "H5Tcopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Copy"
  (dtype-id hid-t))

(cffi:defcfun "H5Tcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Create"
  (class h5t-class-t)
  (size  size-t))

(cffi:defcfun "H5Tdecode" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Decode"
  (buf (:pointer :unsigned-char)))

(cffi:defcfun "H5Tdetect_class" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-DetectClass"
  (dtype-id hid-t)
  (class    h5t-class-t))

(cffi:defcfun "H5Tencode" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Encode"
  (obj-id hid-t)
  (buf    (:pointer :unsigned-char))
  (nalloc (:pointer size-t)))

(cffi:defcfun "H5Tenum_create" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumCreate"
  (dtype-id hid-t))

(cffi:defcfun "H5Tenum_insert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumInsert"
  (dtype-id hid-t)
  (name     :string)
  (value    :pointer))

(cffi:defcfun "H5Tenum_nameof" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumNameOf"
  (dtype-id hid-t)
  (value    :pointer)
  (name     (:pointer :char))
  (size     size-t))

(cffi:defcfun "H5Tenum_valueof" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-EnumValueOf"
  (dtype-id hid-t)
  (name     (:pointer :char))
  (value    (:pointer)))

(cffi:defcfun "H5Tequal" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Equal"
  (dtype-id1 hid-t)
  (dtype-id2 hid-t))

(cffi:defcfun "H5Tfind" :pointer
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Find"
  (src-id hid-t)
  (dst-id hid-t)
  (pcdata (:pointer (:pointer (:struct h5t-cdata-t)))))

(cffi:defcfun "H5Tget_array_dims2" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayDims2"
  (adtype-id hid-t)
  (dims      (:pointer hsize-t)))

(cffi:defcfun "H5Tget_array_ndims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayNdims"
  (adtype-id hid-t))

(cffi:defcfun "H5Tget_class" h5t-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetClass"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCreatePlist"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_cset" h5t-cset-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCset"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_ebias" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetEbias"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_fields" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetFields"
  (dtype-id hid-t)
  (spos     (:pointer size-t))
  (epos     (:pointer size-t))
  (esize    (:pointer size-t))
  (mpos     (:pointer size-t))
  (msize    (:pointer size-t)))

(cffi:defcfun "H5Tget_inpad" h5t-pad-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetFields"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_member_class" h5t-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberClass"
  (cdtype-id hid-t)
  (member-no :unsigned-int))

(cffi:defcfun "H5Tget_member_index" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberIndex"
  (dtype-id   hid-t)
  (field-name :string))

(cffi:defcfun "H5Tget_member_name" (:pointer :char)
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberName"
  (dtype-id  hid-t)
  (field-idx :uint))

(cffi:defcfun "H5Tget_member_offset" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberOffset"
  (dtype-id hid-t)
  (memb-no :uint))

(cffi:defcfun "H5Tget_member_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberType"
  (dtype-id  hid-t)
  (field-idx :uint))

(cffi:defcfun "H5Tget_member_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberValue"
  (dtype-id hid-t)
  (memb-no  :unsigned-int)
  (value    :pointer))

(cffi:defcfun "H5Tget_native_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNativeType"
  (dtype-id  hid-t)
  (direction h5t-dir-t))

(cffi:defcfun "H5Tget_nmembers" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNmembers"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_norm" h5t-norm-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNorm"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_offset" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetOffset"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_order" h5t-order-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetOrder"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_pad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetPad"
  (dtype-id hid-t)
  (lsb      (:pointer h5t-pad-t))
  (msb      (:pointer h5t-pad-t)))

(cffi:defcfun "H5Tget_precision" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetPrecision"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_sign" h5t-sign-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSign"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_size" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSize"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_strpad" h5t-str-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetStrpad"
  (dtype-id hid-t))

(cffi:defcfun "H5Tget_super" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSuper"
  (type hid-t))

(cffi:defcfun "H5Tget_tag" (:pointer :char)
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetTag"
  (dtype-id hid-t))

(cffi:defcfun "H5Tinsert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Insert"
  (dtype-id hid-t)
  (name     :string)
  (offset   size-t)
  (field-id hid-t))

(cffi:defcfun "H5Tis_variable_str" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-IsVariableString"
  (dtype-id hid-t))

(cffi:defcfun "H5Tlock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Lock"
  (dtype-id  hid-t))

(cffi:defcfun "H5Topen2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Open2"
  (loc-id  hid-t)
  (name    :string)
  (tapl-id hid-t))

(cffi:defcfun "H5Tpack" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Pack"
  (dtype-id  hid-t))

(cffi:defcfun "H5Tregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Register"
  (type   h5t-pers-t)
  (name   (:pointer :char))
  (src-id hid-t)
  (dst-id hid-t)
  (func   :pointer))

(cffi:defcfun "H5Tset_cset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetCset"
  (dtype-id hid-t)
  (cset     h5t-cset-t))

(cffi:defcfun "H5Tset_ebias" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetEbias"
  (dtype-id hid-t)
  (ebias    size-t))

(cffi:defcfun "H5Tset_fields" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetFields"
  (dtype-id hid-t)
  (spos     size-t)
  (epos     size-t)
  (esize    size-t)
  (mpos     size-t)
  (msize    size-t))

(cffi:defcfun "H5Tset_inpad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetInpad"
  (dtype-id hid-t)
  (inpad    h5t-pad-t))

(cffi:defcfun "H5Tset_norm" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetNorm"
  (dtype-id hid-t)
  (norm     h5t-norm-t))

(cffi:defcfun "H5Tset_offset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetOffset"
  (dtype-id hid-t)
  (offset   size-t))

(cffi:defcfun "H5Tset_order" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetOrder"
  (dtype-id hid-t)
  (order    h5t-order-t))

(cffi:defcfun "H5Tset_pad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetPad"
  (dtype-id hid-t)
  (lsb      h5t-pad-t)
  (msb      h5t-pad-t))

(cffi:defcfun "H5Tset_precision" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetPrecision"
  (dtype-id  hid-t)
  (precision size-t))

(cffi:defcfun "H5Tset_sign" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetSign"
  (dtype-id hid-t)
  (sign     h5t-sign-t))

(cffi:defcfun "H5Tset_size" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetSize"
  (dtype-id hid-t)
  (size     size-t))

(cffi:defcfun "H5Tset_strpad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetStrpad"
  (dtype-id hid-t)
  (cset     h5t-str-t))

(cffi:defcfun "H5Tset_tag" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetTag"
  (dtype-id hid-t)
  (tag      :string))

(cffi:defcfun "H5Tunregister" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Unregister"
  (type   h5t-pers-t)
  (name   :string)
  (src-id hid-t)
  (dst-id hid-t)
  (func   :pointer))

(cffi:defcfun "H5Tvlen_create" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-VLCreate"
  (base-type-id hid-t))
