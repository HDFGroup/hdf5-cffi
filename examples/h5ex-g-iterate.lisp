;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to iterate over group members using H5Literate.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_iterate.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

;;; Download the input file from
;;; http://www.hdfgroup.org/ftp/HDF5/examples/files/exbyapi/h5ex_g_iterate.h5
;;; or use one of your own files.

(defparameter *FILE* "h5ex_g_iterate.h5")

;;; the callback function

(cffi:defcallback op-func herr-t
    ((loc-id hid-t)
     (name :string)
     (info (:pointer (:struct h5l-info-t)))
     (operator-data :pointer))
  (progn
    (cffi:with-foreign-object (infobuf '(:struct h5o-info-t) 1)
      (h5oget-info-by-name loc-id name
			   (cffi:mem-aptr infobuf '(:struct h5o-info-t) 0)
			   +H5P-DEFAULT+)
      ;; retrieve the object type and display the link name
      (let ((type (cffi:foreign-slot-value infobuf
					   '(:struct h5o-info-t) 'type)))
	(cond ((eql type :H5O-TYPE-GROUP)
	       (format t "  Group: ~S~%" name))
	      ((eql type :H5O-TYPE-DATASET)
	       (format t "  Dataset: ~S~%" name))
	      ((eql type :H5O-TYPE-NAMED-DATATYPE)
	       (format t "  Datatype: ~S~%" name))
	      (t (format t "  Unknown: ~S~%" name)))))
    0))

;;; Showtime

(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2
	       (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
	       (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  
  (unwind-protect
       
       ;; iterate over the links and invoke the callback
       (h5literate file :H5-INDEX-NAME :H5-ITER-NATIVE (cffi:null-pointer)
		   (cffi:callback op-func) (cffi:null-pointer))

    (h5fclose file)
    (h5pclose fapl)))

#+sbcl(sb-ext:quit)
