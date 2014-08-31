;;;; See H5Tpublic.h .

(in-package #:hdf5-cffi)

(defcenum h5t-class-t
  (:H5T-NO-CLASS -1)
  :H5T-INTEGER
  :H5T-FLOAT
  :H5T-TIME
  :H5T-STRING
  :H5T-BITFIELD
  :H5T-OPAQUE
  :H5T-COMPOUND
  :H5T-REFERENCE
  :H5T-ENUM
  :H5T-VLEN
  :H5T-ARRAY
  :H5T-NCLASSES)

(defcenum h5t-order-t
    (:H5T-ORDER-ERROR -1)
    :H5T-ORDER-LE
    :H5T-ORDER-BE
    :H5T-ORDER-VAX
    :H5T-ORDER-MIXED
    :H5T-ORDER-NONE
    :H5T_ORDER-NONE)

(defcenum h5t-sign-t
  (:H5T-SGN-ERROR -1)
  :H5T_SGN_NONE
  :H5T-SGN-2
  :H5T_NSGN)

(defcenum h5t-norm-t
  (:H5T-NORM-ERROR -1)
  :H5T-NORM-IMPLIED
  :H5T-NORM-MSBSET
  :H5T-NORM-NONE)

(defcenum h5t-cset-t
  (:H5T-CSET-ERROR -1)
  :H5T-CSET-ASCII
  :H5T-CSET-UTF8
  :H5T-CSET-RESERVED-2
  :H5T-CSET-RESERVED-3
  :H5T-CSET-RESERVED-4
  :H5T-CSET-RESERVED-5
  :H5T-CSET-RESERVED-6
  :H5T-CSET-RESERVED-7
  :H5T-CSET-RESERVED-8
  :H5T-CSET-RESERVED-9
  :H5T-CSET-RESERVED-10
  :H5T-CSET-RESERVED-11
  :H5T-CSET-RESERVED-12
  :H5T-CSET-RESERVED-13
  :H5T-CSET-RESERVED-14
  :H5T-CSET-RESERVED-15)

(defconstant +H5T-NCSET+ 2)

(defcenum h5t-str-t
  (:H5T-STR-ERROR -1)
  :H5T-STR-NULLTERM
  :H5T-STR-NULLPAD
  :H5T-STR-SPACEPAD
  :H5T-STR-RESERVED-3
  :H5T-STR-RESERVED-4
  :H5T-STR-RESERVED-5
  :H5T-STR-RESERVED-6
  :H5T-STR-RESERVED-7
  :H5T-STR-RESERVED-8
  :H5T-STR-RESERVED-9
  :H5T-STR-RESERVED-10
  :H5T-STR-RESERVED-11
  :H5T-STR-RESERVED-12
  :H5T-STR-RESERVED-13
  :H5T-STR-RESERVED-14
  :H5T-STR-RESERVED-15)

(defconstant +H5T-NSTR+ 3) 

(defcenum h5t-pad-t
  (:H5T_PAD_ERROR -1)
  :H5T_PAD_ZERO
  :H5T_PAD_ONE 
  :H5T_PAD_BACKGROUND
  :H5T_NPAD)

(defcenum h5t-direction-t
  (:H5T-DIR-DEFAULT 0)
  :H5T-DIR-ASCEND
  :H5T-DIR-DESCEND)

(defcstruct hvl-t
  (len size-t)
  (p :pointer))

(defconstant +H5T-VARIABLE+ (1- (ash 1 (* 8 +SIZE-OF-SIZE-T+))))

(defconstant +H5T-OPAQUE-TAG-MAX 256)

;;; predefined HDF5 datatypes

;;; IEEE

(defconstant +H5T-IEEE-F32BE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F32BE_g") 'hid-t))
(defconstant +H5T-IEEE-F32LE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F32LE_g") 'hid-t))
(defconstant +H5T-IEEE-F64BE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F64BE_g") 'hid-t))
(defconstant +H5T-IEEE-F64LE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F64LE_g") 'hid-t))

;;; standard

(defconstant +H5T-STD-I8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I8BE_g") 'hid-t))
(defconstant +H5T-STD-I8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I8LE_g") 'hid-t))
(defconstant +H5T-STD-I16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I16BE_g") 'hid-t))
(defconstant +H5T-STD-I16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I16LE_g") 'hid-t))
(defconstant +H5T-STD-I32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I32BE_g") 'hid-t))
(defconstant +H5T-STD-I32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I32LE_g") 'hid-t))
(defconstant +H5T-STD-I64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I64BE_g") 'hid-t))
(defconstant +H5T-STD-I64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I64LE_g") 'hid-t))

(defconstant +H5T-STD-U8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U8BE_g") 'hid-t))
(defconstant +H5T-STD-U8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U8LE_g") 'hid-t))
(defconstant +H5T-STD-U16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U16BE_g") 'hid-t))
(defconstant +H5T-STD-U16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U16LE_g") 'hid-t))
(defconstant +H5T-STD-U32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U32BE_g") 'hid-t))
(defconstant +H5T-STD-U32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U32LE_g") 'hid-t))
(defconstant +H5T-STD-U64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U64BE_g") 'hid-t))
(defconstant +H5T-STD-U64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U64LE_g") 'hid-t))

(defconstant +H5T-STD-B8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B8BE_g") 'hid-t))
(defconstant +H5T-STD-B8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B8LE_g") 'hid-t))
(defconstant +H5T-STD-B16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B16BE_g") 'hid-t))
(defconstant +H5T-STD-B16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B16LE_g") 'hid-t))
(defconstant +H5T-STD-B32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B32BE_g") 'hid-t))
(defconstant +H5T-STD-B32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B32LE_g") 'hid-t))
(defconstant +H5T-STD-B64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B64BE_g") 'hid-t))
(defconstant +H5T-STD-B64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B64LE_g") 'hid-t))

(defconstant +H5T-STD-REF-OBJ+
  (mem-ref (foreign-symbol-pointer "H5T_STD_REF_OBJ_g") 'hid-t))
(defconstant +H5T-STD-REF-DSETREG+
  (mem-ref (foreign-symbol-pointer "H5T_STD_REF_DSETREG_g") 'hid-t))

;;; UNIX

(if (foreign-symbol-pointer "H5T_UNIX_D32BE_g")
    (defconstant +H5T-UNIX-D32BE+
      (mem-ref (foreign-symbol-pointer "H5T_UNIX_D32BE_g") 'hid-t)))
(if (foreign-symbol-pointer "H5T_UNIX_D32LE_g")
    (defconstant +H5T-UNIX-D32LE+
      (mem-ref (foreign-symbol-pointer "H5T_UNIX_D32LE_g") 'hid-t)))
(if (foreign-symbol-pointer "H5T_UNIX_D64BE_g")
    (defconstant +H5T-UNIX-D64BE+
      (mem-ref (foreign-symbol-pointer "H5T_UNIX_D64BE_g") 'hid-t)))
(if (foreign-symbol-pointer "H5T_UNIX_D64LE_g")
    (defconstant +H5T-UNIX-D64LE+
      (mem-ref (foreign-symbol-pointer "H5T_UNIX_D64LE_g") 'hid-t)))

;;; C and FORTRAN strings

(defconstant +H5T-C-S1+
  (mem-ref (foreign-symbol-pointer "H5T_C_S1_g") 'hid-t))

(defconstant +H5T-FORTRAN-S1+
  (mem-ref (foreign-symbol-pointer "H5T_FORTRAN_S1_g") 'hid-t))

;;; native

(defconstant +H5T-NATIVE-CHAR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_SCHAR_g") 'hid-t))
(defconstant +H5T-NATIVE-UCHAR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_UCHAR_g") 'hid-t))
(defconstant +H5T-NATIVE-SHORT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_SHORT_g") 'hid-t))
(defconstant +H5T-NATIVE-USHORT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_USHORT_g") 'hid-t))
(defconstant +H5T-NATIVE-INT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_INT_g") 'hid-t))
(defconstant +H5T-NATIVE-UINT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_UINT_g") 'hid-t))
(defconstant +H5T-NATIVE-LONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_LONG_g") 'hid-t))
(defconstant +H5T-NATIVE-ULONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_ULONG_g") 'hid-t))
(defconstant +H5T-NATIVE-LLONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_LLONG_g") 'hid-t))
(defconstant +H5T-NATIVE-ULLONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_ULLONG_g") 'hid-t))

(defconstant +H5T-NATIVE-FLOAT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_FLOAT_g") 'hid-t))
(defconstant +H5T-NATIVE-DOUBLE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_DOUBLE_g") 'hid-t))

(defconstant +H5T-NATIVE-B8+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B8_g") 'hid-t))
(defconstant +H5T-NATIVE-B16+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B16_g") 'hid-t))
(defconstant +H5T-NATIVE-B32+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B32_g") 'hid-t))
(defconstant +H5T-NATIVE-B64+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B64_g") 'hid-t))

(defconstant +H5T-NATIVE-OPAQUE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_OPAQUE_g") 'hid-t))
(defconstant +H5T-NATIVE-HADDR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HADDR_g") 'hid-t))
(defconstant +H5T-NATIVE-HSIZE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HSIZE_g") 'hid-t))
(defconstant +H5T-NATIVE-HSSIZE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HSSIZE_g") 'hid-t))
(defconstant +H5T-NATIVE-HERR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HERR_g") 'hid-t))
(defconstant +H5T-NATIVE-HBOOL+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HBOOL_g") 'hid-t))

;;; H5T API functions

(defcfun "H5Tarray_create2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-ArrayCreate2"
  (base-type-id hid-t)
  (rank :uint)
  (dims (:pointer hsize-t)))

(defcfun "H5Tclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Close"
  (dtype-id hid-t))

(defcfun "H5Tcommit2" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Commit"
  (loc-id hid-t)
  (name :string)
  (dtype-id hid-t)
  (lcpl-id hid-t)
  (tcpl-id hid-t)
  (tapl-id hid-t))

(defcfun "H5Tcommit_anon" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-CommitAnon"
  (loc-id hid-t)
  (dtype-id hid-t)
  (tcpl-id hid-t)
  (tapl-id hid-t))

(defcfun "H5Tcommitted" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Committed"
  (dtype-id hid-t))

(defcfun "H5Tcopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Copy"
  (dtype-id hid-t))

(defcfun "H5Tcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Create"
  (class h5t-class-t)
  (size size-t))

(defcfun "H5Tdecode" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Decode"
  (buf (:pointer :unsigned-char)))

(defcfun "H5Tdetect_class" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-DetectClass"
  (dtype-id hid-t)
  (class h5t-class-t))

(defcfun "H5Tencode" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Encode"
  (obj-id hid-t)
  (buf (:pointer :unsigned-char))
  (nalloc (:pointer size-t)))

(defcfun "H5Tequal" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Equal"
  (dtype-id1 hid-t)
  (dtype-id2 hid-t))

(defcfun "H5Tget_array_dims2" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayDims2"
  (adtype-id hid-t)
  (dims (:pointer hsize-t)))

(defcfun "H5Tget_array_ndims" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetArrayNdims"
  (adtype-id hid-t))

(defcfun "H5Tget_class" h5t-class-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetClass"
  (dtype-id hid-t))

(defcfun "H5Tget_create_plist" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCreatePlist"
  (dtype-id hid-t))

(defcfun "H5Tget_cset" h5t-cset-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetCset"
  (dtype-id hid-t))

(defcfun "H5Tget_member_name" :string
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberName"
  (dtype-id hid-t)
  (field-idx :uint))

(defcfun "H5Tget_member_offset" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberOffset"
  (dtype-id hid-t)
  (memb-no :uint))

(defcfun "H5Tget_member_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetMemberType"
  (dtype-id hid-t)
  (field-idx :uint))

(defcfun "H5Tget_native_type" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNativeType"
  (dtype-id hid-t)
  (direction h5t-direction-t))

(defcfun "H5Tget_nmembers" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetNmembers"
  (dtype-id hid-t))

(defcfun "H5Tget_size" size-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSize"
  (dtype-id hid-t))

(defcfun "H5Tget_strpad" h5t-str-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetStrpad"
  (dtype-id hid-t))

(defcfun "H5Tget_super" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSuper"
  (type hid-t))

(defcfun "H5Tget_tag" (:pointer :char)
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetTag"
  (dtype-id hid-t))

(defcfun "H5Tinsert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Insert"
  (dtype-id hid-t)
  (name :string)
  (offset size-t)
  (field-id hid-t))

(defcfun "H5Tis_variable_str" htri-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-IsVariableString"
  (dtype-id hid-t))

(defcfun "H5Topen2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Open2"
  (loc-id hid-t)
  (name :string)
  (tapl-id hid-t))

(defcfun "H5Tset_cset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetCset"
  (dtype-id hid-t)
  (cset h5t-cset-t))

(defcfun "H5Tset_strpad" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetStrpad"
  (dtype-id hid-t)
  (cset h5t-str-t))

(defcfun "H5Tset_tag" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-SetTag"
  (dtype-id hid-t)
  (tag :string))

(defcfun "H5Tvlen_create" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-VLCreate"
  (base-type-id hid-t))
