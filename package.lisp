;;;; package.lisp

(defpackage #:hdf5-cffi
  (:use #:cl #:cffi)
  (:export
   
   ;; == h5 ===================================================================
   
   :haddr-t
   :hbool-t
   :herr-t
   :hsize-t
   :hssize-t
   :htri-t
   :size-t
   :ssize-t
      
   :+HADDR-UNDEF+
   :+HADDR-MAX+
   :+H5P-DEFAULT+
   :+H5-VERS-MAJOR+
   :+H5-VERS-MINOR+
   :+H5-VERS-RELEASE+

   :h5close
   :h5dont-atexit
   :h5free-memory
   :h5garbage-collect
   :h5get-libversion
   :h5open
   :h5set-free-list-limits

   ;; == h5i ==================================================================

   :hid-t
   :h5i-type-t

   :+H5I-INVALID-HID+

   :h5iget-file-id
   :h5iget-name
   :h5iget-type
   :h5iis-valid
   :h5inmembers

   ;; == h5f ==================================================================

   :h5f-intent-flags
   :h5f-scope-t

   :h5fclose
   :h5fcreate
   :h5fflush
   :h5fget-access-plist
   :h5fcreate-plist
   :h5fget-filesize
   :h5fget-intent
   :h5fget-name
   :h5fmount
   :h5fopen
   :h5freopen
   :h5funmount

   ;; == h5t ==================================================================

   :h5t-class-t
   :h5t-cset-t
   :h5t-direction-t
   :h5t-norm-t
   :h5t-order-t
   :h5t-pad-t
   :h5t-sign-t
   :h5t-str-t
   :hvl-t

   :+H5T-NCSET+
   :+H5T-NSTR+
   :+H5T-OPAQUE-TAG-MAX+
   :+H5T-VARIABLE+

   :+H5T-IEEE-F32BE+
   :+H5T-IEEE-F32LE+
   :+H5T-IEEE-F64BE+
   :+H5T-IEEE-F64LE+
   :+H5T-STD-I8BE+
   :+H5T-STD-I8LE+
   :+H5T-STD-I16BE+
   :+H5T-STD-I16LE+
   :+H5T-STD-I32BE+
   :+H5T-STD-I32LE+
   :+H5T-STD-I64BE+
   :+H5T-STD-I64LE+
   :+H5T-STD-U8BE+
   :+H5T-STD-U8LE+
   :+H5T-STD-U16BE+
   :+H5T-STD-U16LE+
   :+H5T-STD-U32BE+
   :+H5T-STD-U32LE+
   :+H5T-STD-U64BE+
   :+H5T-STD-U64LE+
   :+H5T-STD-B8BE+
   :+H5T-STD-B8LE+
   :+H5T-STD-B16BE+
   :+H5T-STD-B16LE+
   :+H5T-STD-B32BE+
   :+H5T-STD-B32LE+
   :+H5T-STD-B64BE+
   :+H5T-STD-B64LE+
   :+H5T-STD-REF-OBJ+
   :+H5T-STD-REF-DSETREG+
   :+H5T-NATIVE-CHAR+
   :+H5T-NATIVE-UCHAR+
   :+H5T-NATIVE-SHORT+
   :+H5T-NATIVE-USHORT+
   :+H5T-NATIVE-INT+
   :+H5T-NATIVE-UINT+
   :+H5T-NATIVE-LONG+
   :+H5T-NATIVE-ULONG+
   :+H5T-NATIVE-LLONG+
   :+H5T-NATIVE-ULLONG+
   :+H5T-NATIVE-FLOAT+
   :+H5T-NATIVE-DOUBLE+
   :+H5T-NATIVE-B8+
   :+H5T-NATIVE-B16+
   :+H5T-NATIVE-B32+
   :+H5T-NATIVE-B64+
   :+H5T-NATIVE-OPAQUE+
   :+H5T-NATIVE-HADDR+
   :+H5T-NATIVE-HSIZE+
   :+H5T-NATIVE-HSSIZE+
   :+H5T-NATIVE-HERR+
   :+H5T-NATIVE-HBOOL+

   :h5tarray-create2
   :h5tclose
   :h5tcreate
   :h5tequal
   :h5tget-array-dims2
   :h5tget-array-ndims
   :h5tget-class
   :h5tget-member-name
   :h5tget-member-offset
   :h5tget-member-type
   :h5tget-native-type
   :h5tget-nmembers
   :h5tget-size
   :h5tinsert
   :h5tget-super))

