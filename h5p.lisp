;;;; h5p.lisp - See H5Ppublic.h

(in-package #:hdf5-cffi)

(defconstant +H5P-ROOT+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_ROOT_g") 'hid-t))
(defconstant +H5P-OBJECT-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_OBJECT_CREATE_g") 'hid-t))
(defconstant +H5P-FILE-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_FILE_CREATE_g") 'hid-t))
(defconstant +H5P-FILE-ACCESS+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_FILE_ACCESS_g") 'hid-t))
(defconstant +H5P-DATASET-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_DATASET_CREATE_g") 'hid-t))
(defconstant +H5P-DATASET-ACCESS+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_DATASET_ACCESS_g") 'hid-t))
(defconstant +H5P-DATASET-XFER+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_DATASET_XFER_g") 'hid-t))
(defconstant +H5P-FILE-MOUNT+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_FILE_MOUNT_g") 'hid-t))
(defconstant +H5P-GROUP-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_GROUP_CREATE_g") 'hid-t))
(defconstant +H5P-GROUP-ACCESS+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_GROUP_ACCESS_g") 'hid-t))
(defconstant +H5P-DATATYPE-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_DATATYPE_CREATE_g") 'hid-t))
(defconstant +H5P-DATATYPE-ACCESS+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_DATATYPE_ACCESS_g") 'hid-t))
(defconstant +H5P-STRING-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_STRING_CREATE_g") 'hid-t))
(defconstant +H5P-ATTRIBUTE-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_ATTRIBUTE_CREATE_g") 'hid-t))
(defconstant +H5P-OBJECT-COPY+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_OBJECT_COPY_g") 'hid-t))
(defconstant +H5P-LINK-CREATE+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_LINK_CREATE_g") 'hid-t))
(defconstant +H5P-LINK-ACCESS+
  (mem-ref (foreign-symbol-pointer "H5P_CLS_LINK_ACCESS_g") 'hid-t))

(defconstant +H5P-FILE-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_FILE_CREATE_g") 'hid-t))
(defconstant +H5P-FILE-ACCESS-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_FILE_ACCESS_g") 'hid-t))
(defconstant +H5P-DATASET-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_DATASET_CREATE_g") 'hid-t))
(defconstant +H5P-DATASET-ACCESS-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_DATASET_ACCESS_g") 'hid-t))
(defconstant +H5P-DATASET-XFER-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_DATASET_XFER_g") 'hid-t))
(defconstant +H5P-FILE-MOUNT-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_FILE_MOUNT_g") 'hid-t))
(defconstant +H5P-GROUP-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_GROUP_CREATE_g") 'hid-t))
(defconstant +H5P-GROUP-ACCESS-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_GROUP_ACCESS_g") 'hid-t))
(defconstant +H5P-DATATYPE-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_DATATYPE_CREATE_g") 'hid-t))
(defconstant +H5P-DATATYPE-ACCESS-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_DATATYPE_ACCESS_g") 'hid-t))
(defconstant +H5P-ATTRIBUTE-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_ATTRIBUTE_CREATE_g") 'hid-t))
(defconstant +H5P-OBJECT-COPY-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_OBJECT_COPY_g") 'hid-t))
(defconstant +H5P-LINK-CREATE-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_LINK_CREATE_g") 'hid-t))
(defconstant +H5P-LINK-ACCESS-DEFAULT+
  (mem-ref (foreign-symbol-pointer "H5P_LST_LINK_ACCESS_g") 'hid-t))

;;; functions

(defcfun "H5Pclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Close"
  (plist hid-t))

(defcfun "H5Pcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Create"
  (cls-id hid-t))

(defcfun "H5Pcopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Copy"
  (plist hid-t))

(defcfun "H5Pget_char_encoding" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCharEncoding"
  (plist-id hid-t)
  (encoding (:pointer h5t-cset-t)))

(defcfun "H5Pget_chunk" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetChunk"
  (plist hid-t)
  (max-ndims :int)
  (dims (:pointer hsize-t)))

(defcfun "H5Pget_class" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetClass"
  (plist hid-t))

(if (foreign-symbol-pointer "H5Pget_core_write_tracking")
    (defcfun "H5Pget_core_write_tracking" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCoreWriteTracking"
      (fapl-id hid-t)
      (is-enabled (:pointer hbool-t))
      (page-size (:pointer size-t))))

(defcfun "H5Pget_create_intermediate_group" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCreateIntermediateGroup"
  (lcpl-d hid-t)
  (crt-intermed-group (:pointer :uint)))

(defcfun "H5Pget_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetExternal"
  (plist hid-t)
  (idx :uint)
  (name-size size-t)
  (name (:pointer :char))
  (offset (:pointer off-t))
  (size (:pointer hsize-t)))

(defcfun "H5Pget_external_count" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetExternalCount"
  (plist hid-t))

(defcfun "H5Pget_fapl_core" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFaplCore"
  (fapl-id hid-t)
  (increment (:pointer size-t))
  (backing-store (:pointer hbool-t)))

(defcfun "H5Pget_fclose_degree" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFcloseDegree"
  (fapl-id hid-t)
  (fc-degree (:pointer h5f-close-degree-t)))

(defcfun "H5Pget_file_image" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFileImage"
  (fapl-id hid-t)
  (buf-ptr-ptr :pointer)
  (buf-len (:pointer size-t)))

(defcfun "H5Pget_fill_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFillValue"
  (plist-id hid-t)
  (type-id hid-t)
  (value :pointer))

(defcfun "H5Pget_filter2" h5z-filter-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFilter2"
  (plist-id hid-t)
  (idx :unsigned-int)
  (flags (:pointer :unsigned-int))
  (cd-nelmts (:pointer size-t))
  (cd-values (:pointer :unsigned-int))
  (namelen size-t)
  (name (:pointer :char))
  (filter-config (:pointer :unsigned-int)))

(defcfun "H5Pget_layout" h5d-layout-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetLayout"
  (plist hid-t))

(defcfun "H5Pget_libver_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetLibverBounds"
  (fapl-id hid-t)
  (libver-low (:pointer h5f-libver-t))
  (libver-high (:pointer h5f-libver-t)))

(defcfun "H5Pget_nfilters" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetNFilters"
  (plist hid-t))

(defcfun "H5Pget_sizes" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetSizes"
  (plist hid-t)
  (sizeof-addr (:pointer size-t))
  (sizeof-size (:pointer size-t)))

(defcfun "H5Pget_userblock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetUserblock"
  (plist hid-t)
  (size (:pointer hsize-t)))

(defcfun "H5Pget_version" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetVersion"
  (plist hid-t)
  (super (:pointer :uint))
  (freelist (:pointer :uint))
  (stab (:pointer :uint))
  (shhdr (:pointer :uint)))

(defcfun "H5Pset_char_encoding" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCharEncoding"
  (plist-id hid-t)
  (encoding h5t-cset-t))

(defcfun "H5Pset_chunk" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetChunk"
  (plist hid-t)
  (ndims :int)
  (dim (:pointer hsize-t)))

(if (foreign-symbol-pointer "H5Pset_core_write_tracking")
    (defcfun "H5Pset_core_write_tracking" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCoreWriteTracking"
      (fapl-id hid-t)
      (is-enabled hbool-t)
      (page-size size-t)))

(defcfun "H5Pset_create_intermediate_group" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCreateIntermediateGroup"
  (lcpl-d hid-t)
  (crt-intermed-group :uint))

(defcfun "H5Pset_deflate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetDeflate"
  (plist-id hid-t)
  (level :uint))

(defcfun "H5Pset_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetExternal"
  (plist hid-t)
  (name :string)
  (offset off-t)
  (size hsize-t))

(defcfun "H5Pset_fapl_core" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFaplCore"
  (fapl-id hid-t)
  (increment size-t)
  (backing-store hbool-t))

(defcfun "H5Pset_fclose_degree" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFcloseDegree"
  (fapl-id hid-t)
  (fc-degree h5f-close-degree-t))

(defcfun "H5Pset_file_image" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFileImage"
  (fapl-id hid-t)
  (buf-ptr :pointer)
  (buf-len size-t))

(defcfun "H5Pset_fill_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFillValue"
  (plist-id hid-t)
  (type-id hid-t)
  (value :pointer))

(defcfun "H5Pset_fletcher32" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFletcher32"
  (plist-id hid-t))

(defcfun "H5Pset_layout" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLayout"
  (plist hid-t)
  (layout h5d-layout-t))

(defcfun "H5Pset_libver_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLibverBounds"
  (fapl-id hid-t)
  (libver-low h5f-libver-t)
  (libver-high h5f-libver-t))

(defcfun "H5Pset_shuffle" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetShuffle"
  (plist-id hid-t))

(if (foreign-symbol-pointer "H5Pset_szip")
    (defcfun "H5Pset_szip" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetSzip"
      (plist hid-t)
      (options-mask :unsigned-int)
      (pixels-per-block :unsigned-int)))

(defcfun "H5Pset_userblock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetUserblock"
  (plist hid-t)
  (size size-t))
