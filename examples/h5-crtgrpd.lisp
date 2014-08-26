;;;; h5-crtgrpd.lisp

;;; This example illustrates how to create a dataset in a group.

(in-package :hdf5-cffi)

(let
    ((f (h5fopen "groups.h5" '(:rdwr) +H5P-DEFAULT+)))
  (let*
      ((s (with-foreign-object (dims 'hsize-t 2)
	    (setf (mem-aref dims 'hsize-t 0) 3
		  (mem-aref dims 'hsize-t 1) 3)
	    (h5screate-simple 2 dims (null-pointer))))

       ;; create a dataset in "MyGroup"
       (d (h5dcreate2 f "/MyGroup/dset1" +H5T-STD-I32BE+ s +H5P-DEFAULT+
		      +H5P-DEFAULT+ +H5P-DEFAULT+)))

    ;; initialize and wite the data
    (let ((rows 3) (cols 3))
      (with-foreign-object (data :int (* rows cols))
	(dotimes (i rows)
	  (dotimes (j cols)
	    (setf (mem-aref data :int (+ (* i cols) j)) (1+ j))))
	(h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)))
    
    (h5dclose d)
    (h5sclose s))
  
  (let*
      ;; open an exisitng group
      ((g (h5gopen2 f "/MyGroup/Group_A" +H5P-DEFAULT+))
       (s (with-foreign-object (dims 'hsize-t 2)
	    (setf (mem-aref dims 'hsize-t 0) 2
		  (mem-aref dims 'hsize-t 1) 10)
	    (h5screate-simple 2 dims (null-pointer))))

       ;; create a second dataset in "Group_A"
       (d (h5dcreate2 g "dset2" +H5T-STD-I32BE+ s +H5P-DEFAULT+
		      +H5P-DEFAULT+ +H5P-DEFAULT+)))

    ;; intialize and write the data
    (let ((rows 2) (cols 10))
      (with-foreign-object (data :int (* rows cols))
	(dotimes (i rows)
	  (dotimes (j cols)
	    (setf (mem-aref data :int (+ (* i cols) j)) (1+ j))))
      
	(h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)))
    
    (h5dclose d)
    (h5sclose s)
    (h5gclose g))
  
  (h5fclose f))

(in-package :cl-user)
