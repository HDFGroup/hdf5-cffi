;;;; h5-crtdat.lisp

;;; This example illustrates how to create a dataset that is a 4 x 6 
;;; array.

(in-package :hdf5-cffi)

(let*
    ((f (h5fcreate "dset.h5" '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+)) ; file
     (s (with-foreign-object (dims 'hsize-t 2)
	  (setf (mem-aref dims 'hsize-t 0) 4)
	  (setf (mem-aref dims 'hsize-t 1) 6)
	  (h5screate-simple 2 dims (null-pointer))))                 ; shape
     (d (h5dcreate2 f "/dset" +H5T-STD-I32BE+ s +H5P-DEFAULT+
		    +H5P-DEFAULT+ +H5P-DEFAULT+)))                   ; dataset
  ;; close all open handles
  (h5dclose d)
  (h5sclose s)
  (h5fclose f))

(in-package :cl-user)
