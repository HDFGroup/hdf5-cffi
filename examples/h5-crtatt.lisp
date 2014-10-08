;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example illustrates how to create an attribute attached to a
;;; dataset. It depends on the HDF5 file created by h5-crtdat.lisp.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtatt.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE* "dset.h5")

(cffi:with-foreign-objects
    ((dims 'hsize-t 1)
     (attr-data :int 2))

  ;; initialize the attribute data
  (setf (cffi:mem-aref attr-data :int 0) 100
	(cffi:mem-aref attr-data :int 1) 200
        (cffi:mem-aref dims 'hsize-t 0) 2)

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fopen *FILE* +H5F-ACC-RDWR+ fapl))))
    
    (unwind-protect
	 
	 (let*
	     ((dset (h5dopen2 file "/dset" +H5P-DEFAULT+))
	      (shape (h5screate-simple 1 dims (cffi:null-pointer)))
	      (att (h5acreate2 dset "Units" +H5T-STD-I32BE+ shape
			       +H5P-DEFAULT+ +H5P-DEFAULT+)))
	   
	   (h5awrite att +H5T-NATIVE-INT+ attr-data)
	   (h5aclose att)
	   (h5sclose shape)
	   (h5dclose dset))

      (h5fclose file)
      (h5pclose fapl))))

#+sbcl(sb-ext:quit)
