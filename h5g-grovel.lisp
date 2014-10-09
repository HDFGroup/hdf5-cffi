;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(include "H5Gpublic.h")

(in-package :hdf5)

(cenum h5g-storage-type-t
       ((:H5G-STORAGE-TYPE-UNKNOWN      "H5G_STORAGE_TYPE_UNKNOWN"))
       ((:H5G-STORAGE-TYPE-SYMBOL-TABLE "H5G_STORAGE_TYPE_SYMBOL_TABLE"))
       ((:H5G-STORAGE-TYPE-COMPACT      "H5G_STORAGE_TYPE_COMPACT"))
       ((:H5G-STORAGE-TYPE-DENSE        "H5G_STORAGE_TYPE_DENSE")))

(cstruct h5g-info-t "H5G_info_t"
         (storage-type "storage_type" :type hdf5::H5G-storage-type-t)
         (nlinks       "nlinks"       :type hdf5::hsize-t)
	 (max-corder   "max_corder"   :type :int64)
	 (mounted      "mounted"      :type hdf5::hbool-t))
