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
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F32BE_g") :int))
(defconstant +H5T-IEEE-F32LE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F32LE_g") :int))
(defconstant +H5T-IEEE-F64BE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F64BE_g") :int))
(defconstant +H5T-IEEE-F64LE+
  (mem-ref (foreign-symbol-pointer "H5T_IEEE_F64LE_g") :int))

;;; standard

(defconstant +H5T-STD-I8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I8BE_g") :int))
(defconstant +H5T-STD-I8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I8LE_g") :int))
(defconstant +H5T-STD-I16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I16BE_g") :int))
(defconstant +H5T-STD-I16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I16LE_g") :int))
(defconstant +H5T-STD-I32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I32BE_g") :int))
(defconstant +H5T-STD-I32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I32LE_g") :int))
(defconstant +H5T-STD-I64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I64BE_g") :int))
(defconstant +H5T-STD-I64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_I64LE_g") :int))

(defconstant +H5T-STD-U8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U8BE_g") :int))
(defconstant +H5T-STD-U8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U8LE_g") :int))
(defconstant +H5T-STD-U16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U16BE_g") :int))
(defconstant +H5T-STD-U16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U16LE_g") :int))
(defconstant +H5T-STD-U32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U32BE_g") :int))
(defconstant +H5T-STD-U32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U32LE_g") :int))
(defconstant +H5T-STD-U64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U64BE_g") :int))
(defconstant +H5T-STD-U64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_U64LE_g") :int))

(defconstant +H5T-STD-B8BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B8BE_g") :int))
(defconstant +H5T-STD-B8LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B8LE_g") :int))
(defconstant +H5T-STD-B16BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B16BE_g") :int))
(defconstant +H5T-STD-B16LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B16LE_g") :int))
(defconstant +H5T-STD-B32BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B32BE_g") :int))
(defconstant +H5T-STD-B32LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B32LE_g") :int))
(defconstant +H5T-STD-B64BE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B64BE_g") :int))
(defconstant +H5T-STD-B64LE+
  (mem-ref (foreign-symbol-pointer "H5T_STD_B64LE_g") :int))

(defconstant +H5T-STD-REF-OBJ+
  (mem-ref (foreign-symbol-pointer "H5T_STD_REF_OBJ_g") :int))
(defconstant +H5T-STD-REF-DSETREG+
  (mem-ref (foreign-symbol-pointer "H5T_STD_REF_DSETREG_g") :int))

;;; native

(defconstant +H5T-NATIVE-CHAR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_SCHAR_g") :int))
(defconstant +H5T-NATIVE-UCHAR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_UCHAR_g") :int))
(defconstant +H5T-NATIVE-SHORT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_SHORT_g") :int))
(defconstant +H5T-NATIVE-USHORT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_USHORT_g") :int))
(defconstant +H5T-NATIVE-INT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_INT_g") :int))
(defconstant +H5T-NATIVE-UINT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_UINT_g") :int))
(defconstant +H5T-NATIVE-LONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_LONG_g") :int))
(defconstant +H5T-NATIVE-ULONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_ULONG_g") :int))
(defconstant +H5T-NATIVE-LLONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_LLONG_g") :int))
(defconstant +H5T-NATIVE-ULLONG+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_ULLONG_g") :int))

(defconstant +H5T-NATIVE-FLOAT+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_FLOAT_g") :int))
(defconstant +H5T-NATIVE-DOUBLE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_DOUBLE_g") :int))

(defconstant +H5T-NATIVE-B8+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B8_g") :int))
(defconstant +H5T-NATIVE-B16+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B16_g") :int))
(defconstant +H5T-NATIVE-B32+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B32_g") :int))
(defconstant +H5T-NATIVE-B64+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_B64_g") :int))

(defconstant +H5T-NATIVE-OPAQUE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_OPAQUE_g") :int))
(defconstant +H5T-NATIVE-HADDR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HADDR_g") :int))
(defconstant +H5T-NATIVE-HSIZE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HSIZE_g") :int))
(defconstant +H5T-NATIVE-HSSIZE+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HSSIZE_g") :int))
(defconstant +H5T-NATIVE-HERR+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HERR_g") :int))
(defconstant +H5T-NATIVE-HBOOL+
  (mem-ref (foreign-symbol-pointer "H5T_NATIVE_HBOOL_g") :int))

;;; H5T API functions

(defcfun "H5Tarray_create2" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-ArrayCreate2"
  (base-type-id hid-t)
  (rank :uint)
  (dims (:pointer hsize-t)))

(defcfun "H5Tclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Close"
  (dtype-id hid-t))

(defcfun "H5Tcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Create"
  (class h5t-class-t)
  (size size-t))

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

(defcfun "H5Tinsert" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-Insert"
  (dtype-id hid-t)
  (name :string)
  (offset size-t)
  (field-id hid-t))

(defcfun "H5Tget_super" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html#Datatype-GetSuper"
  (type hid-t))
