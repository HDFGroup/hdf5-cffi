;;;; h5-extend.lisp

;;; This example how to work with extendible datasets. The dataset
;;; must be chunked in order to be extendible.

(in-package :hdf5-cffi)

(flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array position access
  
  (let ((file-name "extend.h5")
	(dset-name "ExtendibleArray")
	(rows 3) (cols 3)
	(rows-ext 7) (cols-ext 3))
    
    (let*
	((f (h5fcreate file-name '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+))
	 (s (with-foreign-objects ((dims 'hsize-t 2)
				   (maxdims 'hsize-t 2))
	      (setf (mem-aref dims 'hsize-t 0) rows
		    (mem-aref dims 'hsize-t 1) cols)
	      (setf (mem-aref maxdims 'hsize-t 0) +H5S-UNLIMITED+
		    (mem-aref maxdims 'hsize-t 1) +H5S-UNLIMITED+)
	      (h5screate-simple 2 dims maxdims)))
	 (p (h5pcreate +H5P-DATASET-CREATE+))
	 (d (with-foreign-object (chunkdims 'hsize-t 2)
	     (setf (mem-aref chunkdims 'hsize-t 0) 2
		   (mem-aref chunkdims 'hsize-t 1) 5)
	     (h5pset-chunk p 2 chunkdims)
	     (h5dcreate2 f dset-name +H5T-STD-I32BE+ s
			 +H5P-DEFAULT+ p +H5P-DEFAULT+))))

      (with-foreign-object (data :int (* rows cols))
	(dotimes (i rows)
	  (dotimes (j cols)
	    (let ((pos (pos cols i j)))
	      (setf (mem-aref data :int pos) 1))))
	(h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data))

      (with-foreign-object (dims 'hsize-t 2)
	(setf (mem-aref dims 'hsize-t 0) (+ rows rows-ext)
	      (mem-aref dims 'hsize-t 1) cols)
	(h5dset-extent d dims))

      (with-foreign-objects ((offset 'hsize-t 2)
			     (dimsext 'hsize-t 2)
			     (data :int (* rows-ext cols-ext)))
	(setf (mem-aref offset 'hsize-t 0) 3
	      (mem-aref offset 'hsize-t 1) 0)
	(setf (mem-aref dimsext 'hsize-t 0) rows-ext
	      (mem-aref dimsext 'hsize-t 1) cols-ext)

	(dotimes (i rows-ext)
	  (dotimes (j cols-ext)
	    (let ((pos (pos cols-ext i j)))
	      (cond ((= j 0) (setf (mem-aref data :int pos) 2))
		    ((= j 1) (setf (mem-aref data :int pos) 3))
		    ((= j 2) (setf (mem-aref data :int pos) 4))))))
	
	(let ((fs (h5dget-space d))
	      (ms (h5screate-simple 2 dimsext (null-pointer))))
	  
	  (h5sselect-hyperslab fs :H5S-SELECT-SET offset
			       (null-pointer) dimsext (null-pointer))
	  (h5dwrite d +H5T-NATIVE-INT+ ms fs +H5P-DEFAULT+ data)

	  (h5sclose ms)
	  (h5sclose fs)))
	
      ;; TODO: print the dataset
  
      (h5pclose p)
      (h5dclose d)
      (h5sclose s)
      (h5fclose f))))

(in-package :cl-user)
