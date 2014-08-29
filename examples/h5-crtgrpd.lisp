;;;; h5-crtgrpd.lisp

;;; This example illustrates how to create a dataset in a group.
;;; It depends on the HDF5 file created by h5-crtgrpar.lisp.


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

  (let
      ((f (h5fopen *FILE* '(:rdwr) +H5P-DEFAULT+)))

    ;; create a first dataset in group '/MyGroup' 
    (let*
	((s (progn
	      (setf (mem-aref dims 'hsize-t 0) 3
		    (mem-aref dims 'hsize-t 1) 3)
	      (h5screate-simple 2 dims (null-pointer))))
	 (d (h5dcreate2 f "/MyGroup/dset1" +H5T-STD-I32BE+ s +H5P-DEFAULT+
			+H5P-DEFAULT+ +H5P-DEFAULT+)))
      
      (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset1-data)
      (h5dclose d)
      (h5sclose s))

    ;; create a secon dataset in '/MyGroup/Group_A'
    (let*
	((g (h5gopen2 f "/MyGroup/Group_A" +H5P-DEFAULT+))
	 (s (progn
	      (setf (mem-aref dims 'hsize-t 0) 2
		    (mem-aref dims 'hsize-t 1) 10)
	      (h5screate-simple 2 dims (null-pointer))))
	 (d (h5dcreate2 g "dset2" +H5T-STD-I32BE+ s +H5P-DEFAULT+
			+H5P-DEFAULT+ +H5P-DEFAULT+)))

      (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset2-data)
      (h5dclose d)
      (h5sclose s)
      (h5gclose g))
    
    (h5fclose f)))

(in-package :cl-user)
