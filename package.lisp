;;;; package.lisp

(defpackage #:hdf5-cffi
  (:use #:cl #:cffi)
  (:export

   :time-t
   :off-t
   
   ;; == h5 ===================================================================
   
   :haddr-t
   :hbool-t
   :herr-t
   :hsize-t
   :hssize-t
   :htri-t
   :size-t
   :ssize-t

   :h5-ih-info-t
   :h5-index-t
   :h5-iter-order-t
   
   :+H5-ITER-ERROR+
   :+H5-ITER-CONT+
   :+H5-ITER-STOP+
   :+H5-VERS-MAJOR+
   :+H5-VERS-MINOR+
   :+H5-VERS-RELEASE+
   :+HADDR-UNDEF+
   :+HADDR-MAX+
   :+H5P-DEFAULT+
   :+SIZE-OF-HADDR-T+
   :+SIZE-OF-HSIZE-T+
   :+SIZE-OF-HSSIZE-T+
   
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
   :h5f-libver-t
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
   :h5tget-super

   ;; == h5l ===============================================================

   :+H5L-MAX-LINK-NAME-LEN+

   :h5l-type-t
   :h5l-info-t

   :h5lcreate-external
   :h5lcreate-hard
   :h5lcreate-soft
   :h5ldelete
   :h5lexists
   :h5lvisit
   :h5lvisit-by-name

   ;; == h5o ===============================================================

   :h5o-hdr-info-t
   :h5o-info-t
   :h5o-msg-crt-idx-t
   :h5o-type-t
   
   :h5oclose
   :h5ocopy
   :h5oexists-by-name
   :h5oget-info
   :h5oget-info-by-name
   :h5olink
   :h5oopen
   
   ;; == h5s ===============================================================

   :+H5S-ALL+
   :+H5S-MAX-RANK+
   :+H5S-UNLIMITED+

   :h5s-class-t
   :h5s-sel-type
   :h5s-seloper-t

   :h5s.close
   :h5scopy
   :h5screate
   :h5screate-simple
   :h5sget-select-bounds
   :h5sget-select-type
   :h5sget-simple-extent-dims
   :h5sget-simple-extent-ndims
   :h5sget-simple-extent-npoints
   :h5sget-simple-extent-type
   :h5sis-simple
   :h5sselect-hyperslab
   :h5sselect-valid

   ;; == h5d ===============================================================
   
   :h5d-alloc-time-t
   :h5d-fill-time-t
   :h5d-fill-value-t
   :h5d-layout-t
   :h5d-space-status-t
   
   :h5dclose
   :h5dcreate1
   :h5dcreate2
   :h5dcreate-anon
   :h5dfill
   :h5dget-create-plist
   :h5dget-space
   :h5dget-storage-size
   :h5dget-type
   :h5dopen2
   :h5dread
   :h5dset-extent
   :h5dvlen-get-buf-size
   :h5dvlen-reclaim
   :h5dwrite

   ;; == h5g ===============================================================

   :h5g-storage-type-t
   :h5g-info-type-t

   :h5gclose
   :h5gcreate1
   :h5gcreate2
   :h5gcreate-anon
   :h5gcreate-plist
   :h5gget-info
   :h5gget-info-by-name
   :h5gopen1
   :h5gopen2

   ;; == h5a ===============================================================

   :h5a-info-t
   
   :h5aclose
   :h5acreate1
   :h5acreate2
   :h5acreate-by-name
   :h5adelete
   :h5adelete-by-name
   :h5aexists
   :h5aexists-by-name
   :h5aget-create-plist
   :h5aget-info
   :h5aget-info-by-name
   :h5aget-name
   :h5aget-space
   :h5aget-storage-size
   :h5aget-type
   :h5aiterate2
   :h5aopen
   :h5aopen-by-name
   :h5aread
   :h5arename
   :h5arename-by-name
   :h5awrite

   ;; == h5r ===============================================================

   :h5r-type-t

   :+H5R-OBJ-REF-BUF-SIZE+
   :+H5R-DSET-REG-REF-BUF-SIZE+

   :h5rcreate
   :h5rdereference
   :h5rget-name
   :h5rget-obj-type2
   :h5rget-region

   ;; == h5p ==================================================================

   :+H5P-ROOT+
   :+H5P-OBJECT-CREATE+
   :+H5P-FILE-CREATE+
   :+H5P-FILE-ACCESS+
   :+H5P-DATASET-CREATE+
   :+H5P-DATASET-ACCESS+
   :+H5P-DATASET-XFER+
   :+H5P-FILE-MOUNT+
   :+H5P-GROUP-CREATE+
   :+H5P-GROUP-ACCESS+
   :+H5P-DATATYPE-CREATE+
   :+H5P-DATATYPE-ACCESS+
   :+H5P-STRING-CREATE+
   :+H5P-ATTRIBUTE-CREATE+
   :+H5P-OBJECT-COPY+
   :+H5P-LINK-CREATE+
   :+H5P-LINK-ACCESS+
   :+H5P-FILE-CREATE-DEFAULT+
   :+H5P-FILE-ACCESS-DEFAULT+
   :+H5P-DATASET-CREATE-DEFAULT+
   :+H5P-DATASET-ACCESS-DEFAULT+
   :+H5P-DATASET-XFER-DEFAULT+
   :+H5P-FILE-MOUNT-DEFAULT+
   :+H5P-GROUP-CREATE-DEFAULT+
   :+H5P-GROUP-ACCESS-DEFAULT+
   :+H5P-DATATYPE-CREATE-DEFAULT+
   :+H5P-DATATYPE-ACCESS-DEFAULT+
   :+H5P-ATTRIBUTE-CREATE-DEFAULT+
   :+H5P-OBJECT-COPY-DEFAULT+
   :+H5P-LINK-CREATE-DEFAULT+
   :+H5P-LINK-ACCESS-DEFAULT+

   :h5pclose
   :h5pcreate
   :h5pcopy
   :h5pget-char-encoding
   :h5pget-chunk
   :h5pget-class
   :h5pget-create-intermediate-group
   :h5pget-external
   :h5pget-external-count
   :h5pget-fapl-core
   :h5pget-file-image
   :h5pget-fill-value
   :h5pget-layout
   :h5pget-libver-bounds
   :h5pget-sizes
   :h5pget-userblock
   :h5pget-version
   :h5pset-char-encoding
   :h5pset-chunk
   :h5pset-create-intermediate-group
   :h5pset-deflate
   :h5pset-external
   :h5pset-fapl-core
   :h5pset-file-image
   :h5pset-fill-value
   :h5pset-fletcher32
   :h5pset-layout
   :h5pset-libver-bounds
   :h5pset-shuffle
   :h5pset-userblock))

