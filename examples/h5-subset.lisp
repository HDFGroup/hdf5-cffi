;;;; h5-subset.lisp

;;; This example illustrates how to read/write a subset of data (a slab) 
;;; from/to a dataset in an HDF5 file.

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

    (let
	((f (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+)))

      (let*
	  ((s (progn
		(setf (mem-aref dims 'hsize-t 0) *DIM0*
		      (mem-aref dims 'hsize-t 1) *DIM1*)
		(h5screate-simple 2 dims (null-pointer))))
	   (d (h5dcreate2 f *DATASETNAME* +H5T-STD-I32BE+ s +H5P-DEFAULT+
			  +H5P-DEFAULT+ +H5P-DEFAULT+)))

	(flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array position access
	  (dotimes (i *DIM0*)
	    (dotimes (j *DIM1*)
	      (let ((pos (pos *DIM1* i j)))
		(if (< j (/ *DIM1* 2))
		    (setf (mem-aref data :int pos) 1)
		    (setf (mem-aref data :int pos) 2))))))
	(h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)

	;; TODO: print the dataset
  
	(h5dclose d)
	(h5sclose s))
      
      (h5fclose f))

    (let
	((f (h5fopen *FILE* '(:rdwr) +H5P-DEFAULT+)))

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
	 ((d (h5dopen2 f *DATASETNAME* +H5P-DEFAULT+))
	  (s (h5dget-space d))
	  (m (h5screate-simple 2 dimsm (null-pointer))))

	;; select the hyperslab to be written
	(h5sselect-hyperslab s :H5S-SELECT-SET offset stride count block)

	(dotimes (i *DIM0-SUB*)
	    (dotimes (j *DIM1-SUB*)
	      (setf (mem-aref sdata :int (+ (* i *DIM1-SUB*) j)) 5)))

	;; write to hyperslab
	(h5dwrite d +H5T-NATIVE-INT+ m s +H5P-DEFAULT+ sdata)
	;; read back the entire dataset
	(h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)

	;; TODO: print the dataset
      
	(h5sclose m)
	(h5sclose s)
	(h5dclose d))
      
      (h5fclose f)))

(in-package :cl-user)
