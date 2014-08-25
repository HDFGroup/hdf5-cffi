;;;; h5-rdwr.lisp

;;; This example illustrates how to write and read data in an existing
;;; dataset.

(in-package :hdf5-cffi)

(let*
    ((f (h5fopen "dset.h5" '(:rdwr) +H5P-DEFAULT+)) ; open file
     (d (h5dopen2 f "/dset" +H5P-DEFAULT+)))        ; open dataset

  ;; initialize the array to be written
  (with-foreign-object (data :int (* 4 6))
    (dotimes (i 4)
      (dotimes (j 6)
	(setf (mem-aref data :int (+ (* i 6) j)) (+ (* i 6) j 1))))

    ;; write dataset
    (h5dwrite d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data)
    ;; read it back
    (h5dread d +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ data))

  ;; close all open handles
  (h5dclose d)
  (h5fclose f))

(in-package :cl-user)
