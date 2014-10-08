;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to create a dataset that is a 4 x 6 array.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtdat.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE* "dset.h5")

(cffi:with-foreign-object (dims 'hsize-t 2)
  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    
    (unwind-protect

	 (let*
	     ((shape (prog2
		       (setf (cffi:mem-aref dims 'hsize-t 0) 4
			     (cffi:mem-aref dims 'hsize-t 1) 6)
			 (h5screate-simple 2 dims (cffi:null-pointer))))
	      (dset (h5dcreate2 file "/dset" +H5T-STD-I32BE+ shape
				+H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   (h5dclose dset)
	   (h5sclose shape))

      (h5fclose file)
      (h5pclose fapl))))

#+sbcl(sb-ext:quit)
