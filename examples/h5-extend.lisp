;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example how to work with extendible datasets. The dataset
;;; must be chunked in order to be extendible.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_extend.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILENAME* "extend.h5")
(defparameter *DATASETNAME* "ExtendibleArray")
(defparameter *RANK* 2)

(cffi:with-foreign-objects
    ((dims 'hsize-t 2)
     (maxdims 'hsize-t 2)
     (chunk-dims 'hsize-t 2)
     (data :int (* 3 3))
     (size 'hsize-t 2)
     (offset 'hsize-t 2)
     (dimsext 'hsize-t 2)
     (dataext :int (* 7 3))
     (chunk-dimsr 'hsize-t 2)
     (dimsr 'hsize-t 2)
     (rdata :int (* 10 3)))
  
  (setf (cffi:mem-aref dims 'hsize-t 0) 3
	(cffi:mem-aref dims 'hsize-t 1) 3)
  (setf (cffi:mem-aref maxdims 'hsize-t 0) +H5S-UNLIMITED+
	(cffi:mem-aref maxdims 'hsize-t 1) +H5S-UNLIMITED+)
  (setf (cffi:mem-aref chunk-dims 'hsize-t 0) 2
	(cffi:mem-aref chunk-dims 'hsize-t 1) 5)

  (dotimes (i 3)
    (dotimes (j 3)
      (setf (cffi:mem-aref data :int (+ (* i 3) j)) 1)))

  (setf (cffi:mem-aref dimsext 'hsize-t 0) 7
	(cffi:mem-aref dimsext 'hsize-t 1) 3)

  (flet ((pos (cols i j) (+ (* cols i) j)))
    (dotimes (i 7)
      (dotimes (j 3)
	(let ((pos (pos 3 i j)))
	  (cond ((= j 0) (setf (cffi:mem-aref dataext :int pos) 2))
		((= j 1) (setf (cffi:mem-aref dataext :int pos) 3))
		((= j 2) (setf (cffi:mem-aref dataext :int pos) 4)))))))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILENAME* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))

    (unwind-protect

	 (let*
	     ((shape (h5screate-simple *RANK* dims maxdims))
	      (dcpl (h5pcreate +H5P-DATASET-CREATE+))
	      (dset (prog2
			(h5pset-chunk dcpl *RANK* chunk-dims)
			(h5dcreate2 file *DATASETNAME* +H5T-NATIVE-INT+ shape
				    +H5P-DEFAULT+ dcpl +H5P-DEFAULT+))))
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		     data)

	   ;; extend the dataset
	   (setf (cffi:mem-aref size 'hsize-t 0)
		 (+ (cffi:mem-ref dims 'hsize-t 0)
		    (cffi:mem-ref dimsext 'hsize-t 0))
		 (cffi:mem-aref size 'hsize-t 1) 3)
	   (h5dset-extent dset size)

	   (let
	       ((fshape (h5dget-space dset))
		(mshape (h5screate-simple *RANK* dimsext (cffi:null-pointer))))

	     (setf (cffi:mem-aref offset 'hsize-t 0) 3
		   (cffi:mem-aref offset 'hsize-t 1) 0)
	     (h5sselect-hyperslab fshape :H5S-SELECT-SET offset
				  (cffi:null-pointer) dimsext
				  (cffi:null-pointer))

	     (h5dwrite dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+
		       dataext)

	     (h5sclose mshape)
	     (h5sclose fshape))

	   (h5dclose dset)
	   (h5pclose dcpl)
	   (h5sclose shape))
      
      (h5fclose file)
      (h5pclose fapl)))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fopen *FILENAME* +H5F-ACC-RDONLY+ fapl))))
    
    (unwind-protect

	 (let*
	     ((dset (h5dopen2 file *DATASETNAME* +H5P-DEFAULT+))
	      (fshape (h5dget-space dset))
	      (rank (h5sget-simple-extent-ndims fshape))
	      (plist (h5dget-create-plist dset)))

	   (if (eql :H5D-CHUNKED (h5pget-layout plist))
	       (format t "~a" (h5pget-chunk plist rank chunk-dimsr)))

	   (h5sget-simple-extent-dims fshape dimsr (cffi:null-pointer))

	   (let ((mshape (h5screate-simple rank dimsr (cffi:null-pointer))))
	     (h5dread dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+ rdata)
	     
	     ;; TODO: print rdata
	     
	     (h5sclose mshape))
    
	   (h5pclose plist)
	   (h5sclose fshape)
	   (h5dclose dset))

      (h5fclose file)
      (h5pclose fapl))))

#+sbcl(sb-ext:quit)
