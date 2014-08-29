;;;; h5-extend.lisp

;;; This example how to work with extendible datasets. The dataset
;;; must be chunked in order to be extendible.

(in-package :hdf5-cffi)

(defparameter *FILENAME* "extend.h5")
(defparameter *DATASETNAME* "ExtendibleArray")
(defparameter *RANK* 2)

(with-foreign-objects ((dims 'hsize-t 2)
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
  
  (setf (mem-aref dims 'hsize-t 0) 3
	(mem-aref dims 'hsize-t 1) 3)
  (setf (mem-aref maxdims 'hsize-t 0) +H5S-UNLIMITED+
	(mem-aref maxdims 'hsize-t 1) +H5S-UNLIMITED+)
  (setf (mem-aref chunk-dims 'hsize-t 0) 2
	(mem-aref chunk-dims 'hsize-t 1) 5)

  (dotimes (i 3)
    (dotimes (j 3)
      (setf (mem-aref data :int (+ (* i 3) j)) 1)))

  (setf (mem-aref dimsext 'hsize-t 0) 7
	(mem-aref dimsext 'hsize-t 1) 3)

  (flet ((pos (cols i j) (+ (* cols i) j)))
    (dotimes (i 7)
      (dotimes (j 3)
	(let ((pos (pos 3 i j)))
	  (cond ((= j 0) (setf (mem-aref dataext :int pos) 2))
		((= j 1) (setf (mem-aref dataext :int pos) 3))
		((= j 2) (setf (mem-aref dataext :int pos) 4)))))))

  (let*
      ((f (h5fcreate *FILENAME* '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+))
       (s (h5screate-simple *RANK* dims maxdims))
       (p (h5pcreate +H5P-DATASET-CREATE+))
       (d (prog2
	    (h5pset-chunk p *RANK* chunk-dims)
	    (h5dcreate2 f *DATASETNAME* +H5T-NATIVE-INT+ s +H5P-DEFAULT+
			p +H5P-DEFAULT+))))
    
    (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)

    (setf (mem-aref size 'hsize-t 0) (+ (mem-ref dims 'hsize-t 0)
					(mem-ref dimsext 'hsize-t 0))
	  (mem-aref size 'hsize-t 1) 3)

    (h5dset-extent d size)

    (let
	((fs (h5dget-space d))
	 (ms (h5screate-simple *RANK* dimsext (null-pointer))))

      (setf (mem-aref offset 'hsize-t 0) 3
	    (mem-aref offset 'hsize-t 1) 0)
      (h5sselect-hyperslab fs :H5S-SELECT-SET offset (null-pointer)
			   dimsext (null-pointer))

      (h5dwrite d +H5T-NATIVE-INT+ ms fs +H5P-DEFAULT+ dataext)

      (h5sclose ms)
      (h5sclose fs))

    (h5dclose d)
    (h5pclose p)
    (h5sclose s)
    (h5fclose f))

  (let*
      ((f (h5fopen *FILENAME* '(:rdonly) +H5P-DEFAULT+))
       (d (h5dopen2 f *DATASETNAME* +H5P-DEFAULT+))
       (fs (h5dget-space d))
       (rank (h5sget-simple-extent-ndims fs))

       (p (h5dget-create-plist d)))

    (if (eql :H5D-CHUNKED (h5pget-layout p))
	(format t "~a" (h5pget-chunk p rank chunk-dimsr)))

    (h5sget-simple-extent-dims fs dimsr (null-pointer))

    (let
	((ms (h5screate-simple rank dimsr (null-pointer))))
      (h5dread d +H5T-NATIVE-INT+ ms fs +H5P-DEFAULT+ rdata)
      ;; TODO: print rdata
      (h5sclose ms))
    
    (h5pclose p)
    (h5sclose fs)
    (h5dclose d)
    (h5fclose f)))

(in-package :cl-user)
