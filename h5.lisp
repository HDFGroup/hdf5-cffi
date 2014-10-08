;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

#+sbcl(require 'asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+cmu(progn
         (setf debug:*debug-print-level* 11
               debug:*debug-print-length* 25)
         (setf ext:*gc-verbose* nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* (remove ':newitem *features*))
  (pushnew :newitem *features*)
  (hdf5::load-hdf5-foreign-libraries)
  (in-package #:hdf5))

(cffi:defcfun "H5check_version" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-VersCheck"
  (majnum :unsigned-int)
  (minnum :unsigned-int)
  (relnum :unsigned-int))

(cffi:defcfun "H5close" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Close")

(cffi:defcfun "H5dont_atexit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-DontAtExit")

(if (cffi:foreign-symbol-pointer "H5free_memory")
    (cffi:defcfun "H5free_memory" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-FreeMemory"
      (buf :pointer)))

(cffi:defcfun "H5garbage_collect" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-GarbageCollect")

(cffi:defcfun "H5get_libversion" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Version"
  (majnum (:pointer :unsigned-int))
  (minnum (:pointer :unsigned-int))
  (relnum (:pointer :unsigned-int)))

(cffi:defcfun "H5open" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open")

(cffi:defcfun "H5set_free_list_limits" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-SetFreeListLimits"
  (reg-global-lim :int)
  (reg-list-lim   :int)
  (arr-global-lim :int)
  (arr-list-lim   :int)
  (blk-global-lim :int)
  (blk-list-lim   :int))
