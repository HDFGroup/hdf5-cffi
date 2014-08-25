;;;; h5-crtatt.lisp

;;; This example illustrates how to create an attribute attached to a
;;; dataset.

(in-package :hdf5-cffi)

(let*
    ((f (h5fopen "dset.h5" '(:rdwr) +H5P-DEFAULT+))  ; open file
     (d (h5dopen2 f "/dset" +H5P-DEFAULT+))          ; open dataset
     (s (with-foreign-object (dims 'hsize-t 1)
	  (setf (mem-aref dims :int 0) 2)
	  (h5screate-simple 1 dims (null-pointer)))) ; create shape
     (a (h5acreate2 d "Units" +H5T-STD-I32BE+ s
		    +H5P-DEFAULT+ +H5P-DEFAULT+)))   ; create attribute

  ;; initialize the attribute dims and data
  (with-foreign-object (data :int 2)
    (setf (mem-aref data :int 0) 100
	  (mem-aref data :int 1) 200)
    ;; write the attribute value
    (h5awrite a +H5T-NATIVE-INT+ data))

  ;; close all open handles
  (h5aclose a)
  (h5dclose d)
  (h5fclose f))

(in-package :cl-user)
