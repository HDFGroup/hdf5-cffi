;;;; h5-cmprss.lisp

;;; This example illustrates how to create a compressed dataset.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_cmprss.c

(in-package :hdf5-cffi)

(defparameter *FILE* "cmprss.h5")
(defparameter *RANK* 2)
(defparameter *DIM0* 100)
(defparameter *DIM1* 20)

(with-foreign-objects ((dims 'hsize-t 2)
		       (cdims 'hsize-t 2)
		       (buf :int (* *DIM0* *DIM1*))
		       (flags :uint 1)
		       (info :uint 1)
		       (nelmts 'size-t 1)
		       (rbuf :int (* *DIM0* *DIM1*)))

  (let*
      ((f (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+))
       (s (progn
	    (setf (mem-aref dims 'hsize-t 0) *DIM0*
		  (mem-aref dims 'hsize-t 1) *DIM1*)
	    (h5screate-simple 2 dims (null-pointer))))
       (p (h5pcreate +H5P-DATASET-CREATE+))
       (d (progn
	    (setf (mem-aref cdims 'hsize-t 0) 20
		  (mem-aref cdims 'hsize-t 1) 20)
	    (h5pset-chunk p 2 cdims)
	    (h5pset-deflate p 6)
	    (h5dcreate2 f "Compressed_Data" +H5T-STD-I32BE+ s
		      +H5P-DEFAULT+ p +H5P-DEFAULT+))))

    (dotimes (i *DIM0*)
      (dotimes (j *DIM1*)
	(setf (mem-aref buf :int (+ (* i *DIM1*) j)) (+ i j))))

    (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ buf)
    
    (h5dclose d)
    (h5pclose p)
    (h5sclose s)
    (h5fclose f))

  (let*
      ((f (h5fopen *FILE* '(:rdwr) +H5P-DEFAULT+))
       (d (h5dopen2 f "Compressed_Data" +H5P-DEFAULT+))
       (p (h5dget-create-plist d))
       (numfilt (H5Pget-nfilters p)))

    (dotimes (i numfilt)
      (setf (mem-aref nelmts 'size-t 0) 0)
      (let
	  ((filter-type (h5pget-filter2 p i (mem-aptr flags :uint 0)
					(mem-aptr nelmts 'size-t 0)
					(null-pointer) 0 (null-pointer)
					(mem-aptr info :uint 0))))
	(format t "~S "
		(if (eql filter-type +H5Z-FILTER-DEFLATE+)
		    'H5Z_FILTER_DEFLATE
		    (if (eql filter-type +H5Z-FILTER-SZIP+)
			'H5Z_FILTER_SZIP
			'OTHER)))))

    (h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rbuf)
    
    (h5pclose p)
    (h5dclose d)
    (h5fclose f)))
