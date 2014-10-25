;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to create intermediate groups with
;;; a single call to H5Gcreate.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_intermediate.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE* "h5ex_g_intermediate.h5")

;;; the callback function for H5Ovisit

(cffi:defcallback op-func herr-t
    ((loc-id        hid-t)
     (name          :string)
     (info          (:pointer (:struct h5o-info-t)))
     (operator-data :pointer))
    
    (prog2
	(let
	    ((type (cffi:foreign-slot-value info
					    '(:struct h5o-info-t)
					    'type)))
	  (format t "/")
	  (if (equal name ".")
	      (format t "  (Group)~%")
	      (cond
		((eql type :H5O-TYPE-GROUP)
		 (format t "~S  (Group)~%" name))
		((eql type :H5O-TYPE-DATASET)
		 (format t "~S  (Dataset)~%" name))
		((eql type :H5O-TYPE-NAMED-DATATYPE)
		 (format t "~S  (Datatype)~%" name))
		(t (format t "~S  (Unknown)~%" name)))))
	0))

;;; Showtime

(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2
	       (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
	       (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl)))
     (gcpl (h5pcreate +H5P-LINK-CREATE+))
     (group (prog2
		(h5pset-create-intermediate-group gcpl 1)
		;; Create the group /G1/G2/G3. Note that /G1 and
		;; /G1/G2 do not exist yet. This call would cause
		;; an error if we did not use the previously created
		;; property list.
		(h5gcreate2 file "/G1/G2/G3" gcpl
			    +H5P-DEFAULT+ +H5P-DEFAULT+))))
  
  (unwind-protect
       (progn
	 (format t "Objects in the file:~%")
	 (h5ovisit file :H5-INDEX-NAME :H5-ITER-NATIVE
		   (cffi:callback op-func) (cffi:null-pointer)))
	   
    (h5gclose group)
    (h5pclose gcpl)
    (h5fclose file)
    (h5pclose fapl)))

#+sbcl(sb-ext:quit)
