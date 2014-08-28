;;;; h5-rdwr.lisp

;;; This example illustrates how to write and read data in an existing
;;; dataset. It depends on the HDF5 file created by h5-crtdat.lisp.

(in-package :hdf5-cffi)

(defparameter *FILE* "dset.h5")

(with-foreign-object (dset-data :int (* 4 6))

  ;; initialize the array to be written
  (flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array access
    (dotimes (i 4)
      (dotimes (j 6)
	(let ((pos (pos 6 i j)))
	  (setf (mem-aref dset-data :int pos) (1+ pos))))))

  (let*
      ((f (h5fopen *FILE* '(:rdwr) +H5P-DEFAULT+)) ; open the file
       (d (h5dopen2 f "/dset" +H5P-DEFAULT+)))     ; open the dataset

    ;; write dataset and read it back
    (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset-data)
    (h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset-data)

    ;; close all open handles
    (h5dclose d)
    (h5fclose f)))

(in-package :cl-user)
