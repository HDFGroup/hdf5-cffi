;;;; h5-rdwr.lisp

;;; This example illustrates how to write and read data in an existing
;;; dataset. It depends on the HDF5 file created by h5-crtdat.lisp.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_rdwt.c

(in-package :hdf5-cffi)

(defparameter *FILE* "dset.h5")

(with-foreign-object (dset-data :int (* 4 6))

  ;; initialize the data to be written
  (flet ((pos (cols i j) (+ (* cols i) j))) ; 2D array access function
    (dotimes (i 4)
      (dotimes (j 6)
	(let ((pos (pos 6 i j)))
	  (setf (mem-aref dset-data :int pos) (1+ pos))))))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 ;; open the file
		 (h5fopen *FILE* '(:rdwr) fapl))))
    (unwind-protect
	 (let
	     ((dset (h5dopen2 file "/dset" +H5P-DEFAULT+))) ; open the dataset
	   ;; write the dataset
	   (h5dwrite dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset-data)
	   ;; and read it back
	   (h5dread  dset +H5T-NATIVE-INT+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ dset-data)
	   (h5dclose dset))
      ;;cleanup forms
      (h5fclose file)
      (h5pclose fapl))))

(in-package :cl-user)
