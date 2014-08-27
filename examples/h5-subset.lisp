;;;; h5-subset.lisp

;;; This example illustrates how to read/write a subset of data (a slab) 
;;; from/to a dataset in an HDF5 file.

(in-package :hdf5-cffi)

(flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array position access
  
  (let ((file-name "subset.h5")
	(dset-name "IntArray")
	(rows 8)
	(cols 10)
	(sub-rows 3)
	(sub-cols 4))
    
    (let*
	((f (h5fcreate file-name '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+))
	 (s (with-foreign-object (dims 'hsize-t 2)
	      (setf (mem-aref dims 'hsize-t 0) rows
		    (mem-aref dims 'hsize-t 1) cols)
	      (h5screate-simple 2 dims (null-pointer))))
	 (d (h5dcreate2 f dset-name +H5T-STD-I32BE+ s +H5P-DEFAULT+
			+H5P-DEFAULT+ +H5P-DEFAULT+)))

      (with-foreign-object (data :int (* rows cols))
	(dotimes (i rows)
	  (dotimes (j cols)
	    (let ((pos (pos cols i j)))
	      (if (< j (/ cols 2))
		  (setf (mem-aref data :int pos) 1)
		  (setf (mem-aref data :int pos) 2)))))
	(h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data))

      ;; TODO: print the dataset
  
      (h5dclose d)
      (h5sclose s)
      (h5fclose f))

    (let*
	((f (h5fopen file-name '(:rdwr) +H5P-DEFAULT+))
	 (d (h5dopen2 f dset-name +H5P-DEFAULT+))
	 (s (h5dget-space d))
	 (m (with-foreign-object (dims 'hsize-t 2)
	      (setf (mem-aref dims 'hsize-t 0) sub-rows 
		    (mem-aref dims 'hsize-t 1) sub-cols)
	      (h5screate-simple 2 dims (null-pointer)))))
  
      (with-foreign-objects ((offset 'hsize-t 2)
			     (count 'hsize-t 2)
			     (stride 'hsize-t 2)
			     (block 'hsize-t 2)
			     (sdata :int (* sub-rows sub-cols))
			     (rdata :int (* rows cols)))

	;; initialize the data to be written
	(dotimes (i sub-rows)
	  (dotimes (j sub-cols)
	    (let ((pos (pos sub-cols i j)))
	      (setf (mem-aref sdata :int pos) 5))))

	;; initialize the hyperslab parameters
	(setf (mem-aref offset 'hsize-t 0) 1
	      (mem-aref offset 'hsize-t 1) 2)
	(setf (mem-aref count 'hsize-t 0) sub-rows
	      (mem-aref count 'hsize-t 1) sub-cols)
	(setf (mem-aref stride 'hsize-t 0) 1
	      (mem-aref stride 'hsize-t 1) 1)
	(setf (mem-aref block 'hsize-t 0) 1
	      (mem-aref block 'hsize-t 1) 1)

	;; select the hyperslab to be written
	(h5sselect-hyperslab s :H5S-SELECT-SET offset stride count block)
      
	(h5dwrite d +H5T-NATIVE-INT+ m s +H5P-DEFAULT+ sdata)
	(h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata))
      ;; TODO: print the data

      (h5sclose m)
      (h5sclose s)
      (h5dclose d)
      (h5fclose f))))

(in-package :cl-user)
