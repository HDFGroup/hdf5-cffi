;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example illustrates how to create a compressed dataset.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_cmprss.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE* "cmprss.h5")
(defparameter *RANK* 2)
(defparameter *DIM0* 100)
(defparameter *DIM1* 20)

(defparameter *USE-SZIP* nil)

(cffi:with-foreign-objects ((dims 'hsize-t 2)
			    (cdims 'hsize-t 2)
			    (buf :int (* *DIM0* *DIM1*))
			    (flags :uint 1)
			    (info :uint 1)
			    (nelmts 'size-t 1)
			    (rbuf :int (* *DIM0* *DIM1*))
			    (szip-options-mask :uint 1)
			    (szip-pixels-per-block :uint 1))
  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let*
	     ((shape (progn
		   (setf (cffi:mem-aref dims 'hsize-t 0) *DIM0*
			 (cffi:mem-aref dims 'hsize-t 1) *DIM1*)
		   (h5screate-simple 2 dims (cffi:null-pointer))))
	      (dcpl (h5pcreate +H5P-DATASET-CREATE+))
	      (dset (progn
		      (setf (cffi:mem-aref cdims 'hsize-t 0) 20
			    (cffi:mem-aref cdims 'hsize-t 1) 20)
		      ;; set chunking
		      (h5pset-chunk dcpl 2 cdims)

		      (cond ((not *USE-SZIP*)         ; use GZIP comression
			     (h5pset-deflate dcpl 6))
			    (t                        ; use SZIP compression
			     (setf (cffi:mem-aref szip-options-mask :uint 0)
				   +H5-SZIP-NN-OPTION-MASK+
				   (cffi:mem-aref szip-pixels-per-block :uint 0)
				   16)
			     (h5pset-szip
			      dcpl
			      (cffi:mem-ref szip-options-mask :uint 0)
			      (cffi:mem-ref szip-pixels-per-block :uint 0))))

		      ;; create the dataset
		      (h5dcreate2 file "Compressed_Data" +H5T-STD-I32BE+
				  shape +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))

	   (dotimes (i *DIM0*)
	     (dotimes (j *DIM1*)
	       (setf (cffi:mem-aref buf :int (+ (* i *DIM1*) j)) (+ i j))))

	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     buf)
    
	   (h5dclose dset)
	   (h5pclose dcpl)
	   (h5sclose shape))
      
      ;; cleanup forms
      (h5fclose file)
      (h5pclose fapl)))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
	 (let*
	     ((dset (h5dopen2 file "Compressed_Data" +H5P-DEFAULT+))
	      (plist (h5dget-create-plist dset))
	      (numfilt (H5Pget-nfilters plist)))

	   (dotimes (i numfilt)
	     (setf (cffi:mem-aref nelmts 'size-t 0) 0)
	     ;; check the filter type(s)
	     (let
		 ((filter-type (h5pget-filter2 plist i
					       (cffi:mem-aptr flags :uint 0)
					       (cffi:mem-aptr nelmts 'size-t 0)
					       (cffi:null-pointer) 0
					       (cffi:null-pointer)
					       (cffi:mem-aptr info :uint 0))))
	       (format t "~S "
		       (if (eql filter-type +H5Z-FILTER-DEFLATE+)
			   'H5Z_FILTER_DEFLATE
			   (if (eql filter-type +H5Z-FILTER-SZIP+)
			       'H5Z_FILTER_SZIP
			       'OTHER)))))

	   ;; read the data back
	   (h5dread dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		    rbuf)
    
	   (h5pclose plist)
	   (h5dclose dset))
      
      ;; cleanup forms
      (h5fclose file)
      (h5pclose fapl))))

#+cmu(ext:quit)
#+sbcl(sb-ext:quit)
