;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example illustrates how to read/write a subset of data (a slab) 
;;; from/to a dataset in an HDF5 file.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_subset.c

(in-package :hdf5-cffi)

(defparameter *FILE* "subset.h5")
(defparameter *DATASETNAME* "IntArray")
(defparameter *RANK* 2)

(defparameter *DIM0-SUB* 3)
(defparameter *DIM1-SUB* 4)

(defparameter *DIM0* 8)
(defparameter *DIM1* 10)

(with-foreign-objects ((dims 'hsize-t 2)
		       (dimsm 'hsize-t 2)
		       (data :int (* *DIM0* *DIM1*))
		       (sdata :int (* *DIM0-SUB* *DIM1-SUB*))
		       (rdata :int (* *DIM0* *DIM1*))
		       (count 'hsize-t 2)
		       (offset 'hsize-t 2)
		       (stride 'hsize-t 2)
		       (block 'hsize-t 2))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ fapl))))
    (unwind-protect
	 (let*
	     ((shape (prog2
			 (setf (mem-aref dims 'hsize-t 0) *DIM0*
			       (mem-aref dims 'hsize-t 1) *DIM1*)
			 (h5screate-simple 2 dims (null-pointer))))
	      (dset (h5dcreate2 file *DATASETNAME* +H5T-STD-I32BE+ shape
				+H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))

	   (flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array position access
	     (dotimes (i *DIM0*)
	       (dotimes (j *DIM1*)
		 (let ((pos (pos *DIM1* i j)))
		   (if (< j (/ *DIM1* 2))
		       (setf (mem-aref data :int pos) 1)
		       (setf (mem-aref data :int pos) 2))))))
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)
	   
	   ;; TODO: print the dataset
 
	   (h5dclose dset)
	   (h5sclose shape))

      ;; cleanup forms
      (h5fclose file)
      (h5pclose fapl)))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fopen *FILE* '(:rdwr) fapl))))
    (unwind-protect
	 (progn
	   ;; initialize the hyperslab parameters
	   (setf (mem-aref offset 'hsize-t 0) 1
		 (mem-aref offset 'hsize-t 1) 2)
	   (setf (mem-aref count 'hsize-t 0) *DIM0-SUB*
		 (mem-aref count 'hsize-t 1) *DIM1-SUB*)
	   (setf (mem-aref stride 'hsize-t 0) 1
		 (mem-aref stride 'hsize-t 1) 1)
	   (setf (mem-aref block 'hsize-t 0) 1
		 (mem-aref block 'hsize-t 1) 1)
      
	   ;; memory space extent
	   (setf (mem-aref dimsm 'hsize-t 0) *DIM0-SUB* 
		 (mem-aref dimsm 'hsize-t 1) *DIM1-SUB*)

	   (let*
	       ((dset (h5dopen2 file *DATASETNAME* +H5P-DEFAULT+))
		(fshape (h5dget-space dset))
		(mshape (h5screate-simple 2 dimsm (null-pointer))))

	     ;; select the hyperslab to be written
	     (h5sselect-hyperslab fshape :H5S-SELECT-SET
				  offset stride count block)

	     (dotimes (i *DIM0-SUB*)
	       (dotimes (j *DIM1-SUB*)
		 (setf (mem-aref sdata :int (+ (* i *DIM1-SUB*) j)) 5)))

	     ;; write to hyperslab selection
	     (h5dwrite dset +H5T-NATIVE-INT+ mshape fshape +H5P-DEFAULT+ sdata)
	     ;; read back the entire dataset
	     (h5dread dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
		      rdata)

	     ;; TODO: print the dataset
      
	     (h5sclose mshape)
	     (h5sclose fshape)
	     (h5dclose dset)))
    
      ;; cleanup forms
      (h5fclose file)
      (h5pclose fapl))))

(in-package :cl-user)
