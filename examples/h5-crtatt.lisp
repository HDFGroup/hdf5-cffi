;;;; h5-crtatt.lisp

;;; This example illustrates how to create an attribute attached to a
;;; dataset. It depends on the HDF5 file created by h5-crtdat.lisp.

(in-package :hdf5-cffi)

(defparameter *FILE* "dset.h5")

(with-foreign-objects ((dims 'hsize-t 1)
		       (attr-data :int 2))

  ;; initialize the attribute data
  (setf (mem-aref attr-data :int 0) 100
	(mem-aref attr-data :int 1) 200
        (mem-aref dims 'hsize-t 0) 2)

  (let*
      ((f (h5fopen *FILE* '(:rdwr) +H5P-DEFAULT+))   ; open the file
       (d (h5dopen2 f "/dset" +H5P-DEFAULT+))        ; open the dataset
       (s (h5screate-simple 1 dims (null-pointer)))  ; create a shape
       (a (h5acreate2 d "Units" +H5T-STD-I32BE+ s
		      +H5P-DEFAULT+ +H5P-DEFAULT+))) ; create an attribute

    ;; write the attribute data
    (h5awrite a +H5T-NATIVE-INT+ attr-data)

    ;; close all open handles
    (h5aclose a)
    (h5sclose s)
    (h5dclose d)
    (h5fclose f)))

(in-package :cl-user)
