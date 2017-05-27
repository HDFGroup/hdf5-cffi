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

(include #.hdf5::*hdf5-header-file*)

(cc-flags #.hdf5::*hdf5-cc-flags*)

(in-package #:hdf5)

(constant (+H5P-DEFAULT+ "H5P_DEFAULT"))

(constant (+H5P-ROOT+             "H5P_ROOT"))
(constant (+H5P-OBJECT-CREATE+    "H5P_OBJECT_CREATE"))
(constant (+H5P-FILE-CREATE+      "H5P_FILE_CREATE"))
(constant (+H5P-FILE-ACCESS+      "H5P_FILE_ACCESS"))
(constant (+H5P-DATASET-CREATE+   "H5P_DATASET_CREATE"))
(constant (+H5P-DATASET-ACCESS+   "H5P_DATASET_ACCESS"))
(constant (+H5P-DATASET-XFER+     "H5P_DATASET_XFER"))
(constant (+H5P-FILE-MOUNT+       "H5P_FILE_MOUNT"))
(constant (+H5P-GROUP-CREATE+     "H5P_GROUP_CREATE"))
(constant (+H5P-GROUP-ACCESS+     "H5P_GROUP_ACCESS"))
(constant (+H5P-DATATYPE-CREATE+  "H5P_DATATYPE_CREATE"))
(constant (+H5P-DATATYPE-ACCESS+  "H5P_DATATYPE_ACCESS"))
(constant (+H5P-STRING-CREATE+    "H5P_STRING_CREATE"))
(constant (+H5P-ATTRIBUTE-CREATE+ "H5P_ATTRIBUTE_CREATE"))
(constant (+H5P-OBJECT-COPY+      "H5P_OBJECT_COPY"))
(constant (+H5P-LINK-CREATE+      "H5P_LINK_CREATE"))
(constant (+H5P-LINK-ACCESS+      "H5P_LINK_ACCESS"))

(constant (+H5P-FILE-CREATE-DEFAULT+      "H5P_FILE_CREATE_DEFAULT"))
(constant (+H5P-FILE-ACCESS-DEFAULT+      "H5P_FILE_ACCESS_DEFAULT"))
(constant (+H5P-DATASET-CREATE-DEFAULT+   "H5P_DATASET_CREATE_DEFAULT"))
(constant (+H5P-DATASET-ACCESS-DEFAULT+   "H5P_DATASET_ACCESS_DEFAULT"))
(constant (+H5P-DATASET-XFER-DEFAULT+     "H5P_DATASET_XFER_DEFAULT"))
(constant (+H5P-FILE-MOUNT-DEFAULT+       "H5P_FILE_MOUNT_DEFAULT"))
(constant (+H5P-GROUP-CREATE-DEFAULT+     "H5P_GROUP_CREATE_DEFAULT"))
(constant (+H5P-GROUP-ACCESS-DEFAULT+     "H5P_GROUP_ACCESS_DEFAULT"))
(constant (+H5P-DATATYPE-CREATE-DEFAULT+  "H5P_DATATYPE_CREATE_DEFAULT"))
(constant (+H5P-DATATYPE-ACCESS-DEFAULT+  "H5P_DATATYPE_ACCESS_DEFAULT"))
(constant (+H5P-ATTRIBUTE-CREATE-DEFAULT+ "H5P_ATTRIBUTE_CREATE_DEFAULT"))
(constant (+H5P-OBJECT-COPY-DEFAULT+      "H5P_OBJECT_COPY_DEFAULT"))
(constant (+H5P-LINK-CREATE-DEFAULT+      "H5P_LINK_CREATE_DEFAULT"))
(constant (+H5P-LINK-ACCESS-DEFAULT+      "H5P_LINK_ACCESS_DEFAULT"))

(constant (+H5P-CRT-ORDER-TRACKED+ "H5P_CRT_ORDER_TRACKED"))
(constant (+H5P-CRT-ORDER-INDEXED+ "H5P_CRT_ORDER_INDEXED"))
