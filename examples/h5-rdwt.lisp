;;;; h5-rdwr.lisp

;;; This example illustrates how to write and read data in an existing
;;; dataset.

(in-package :hdf5-cffi)

(let*
    ((f (h5fopen "dset.h5" '(:rdwr) +H5P-DEFAULT+)) ; open file
     (d (h5dopen2 f "/dset" +H5P-DEFAULT+)))        ; open dataset

  ;; initialize the array to be written
  (let ((rows 4) (cols 6))
    (with-foreign-object (data :int (* rows cols))
      (flet ((pos (cols i j) (+ (* cols i) j)))     ; 2D array access
	(dotimes (i rows)
	  (dotimes (j cols)
	    (let ((pos (pos cols i j)))
	      (setf (mem-aref data :int pos) (1+ pos))))))

    ;; write dataset
    (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)
    ;; read it back
    (h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)))

  ;; close all open handles
  (h5dclose d)
  (h5fclose f))

(in-package :cl-user)
