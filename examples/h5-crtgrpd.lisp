;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example illustrates how to create a dataset in a group.
;;; It depends on the HDF5 file created by h5-crtgrpar.lisp.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtgrpd.c

(in-package :hdf5-cffi)

(defparameter *FILE* "groups.h5")

(with-foreign-objects ((dims 'hsize-t 2)
		       (dset1-data :int (* 3 3))
		       (dset2-data :int (* 2 10)))
  
  ;; initialize the data of the first dataset
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (mem-aref dset1-data :int (+ (* i 3) j)) (1+ j))))
  
  ;; initialize the data of the second dataset
  (dotimes (i 2)
    (dotimes (j 10)
      (setf (mem-aref dset2-data :int (+ (* i 10) j)) (1+ j))))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fopen *FILE* '(:rdwr) fapl))))
    (unwind-protect
	 (progn
	   ;; create a first dataset in group '/MyGroup' 
	   (let*
	       ((shape (prog2
			   (setf (mem-aref dims 'hsize-t 0) 3
				 (mem-aref dims 'hsize-t 1) 3)
			   (h5screate-simple 2 dims (null-pointer))))
		(dset (h5dcreate2 file "/MyGroup/dset1" +H5T-STD-I32BE+ shape
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     ;; write to the first dataset
	     (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		       dset1-data)
	     (h5dclose dset)
	     (h5sclose shape))

	   ;; create a second dataset in '/MyGroup/Group_A'
	   (let*
	       ((grp (h5gopen2 file "/MyGroup/Group_A" +H5P-DEFAULT+))
		(shape (prog2
			 (setf (mem-aref dims 'hsize-t 0) 2
			       (mem-aref dims 'hsize-t 1) 10)
			 (h5screate-simple 2 dims (null-pointer))))
		(dset (h5dcreate2 grp "dset2" +H5T-STD-I32BE+ shape
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     ;; write to the second dataset
	     (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		       dset2-data)
	     (h5dclose dset)
	     (h5sclose shape)
	     (h5gclose grp)))

      ;; cleanup forms
      (h5fclose file)
      (h5pclose fapl))))

(in-package :cl-user)
