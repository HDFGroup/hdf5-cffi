;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

;;; This example shows how to read and write variable-length
;;; string datatypes to a dataset.  The program first writes
;;; variable-length strings to a dataset with a dataspace of
;;; DIM0, then closes the file.  Next, it reopens the file,
;;; reads back the data, and outputs it to the screen.

;;; See h5ex_t_vlstring.c at http://www.hdfgroup.org/HDF5/examples/api18-c.html

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE* "h5ex_t_vlstring.h5")
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)

(cffi:with-foreign-objects ((dims 'hsize-t 1))
  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2
		   (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
    
    (unwind-protect
	 (let* ((wdata (cffi:foreign-alloc
			:string :initial-contents
			'("Parting" "is such" "sweet" "sorrow")))
		(ftype (h5tcopy +H5T-FORTRAN-S1+))
		(mtype (h5tcopy +H5T-C-S1+))
		(shape (prog2
			   (setf (cffi:mem-aref dims 'hsize-t 0) *DIM0*)
			   (h5screate-simple 1 dims +NULL+)))
		(dset (prog2
			(h5tset-size ftype +H5T-VARIABLE+)
			(h5dcreate2 file *DATASET* ftype shape +H5P-DEFAULT+
				    +H5P-DEFAULT+ +H5P-DEFAULT+))))

	   (h5tset-size mtype +H5T-VARIABLE+)
	   (h5dwrite dset mtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)
	   
	   (h5dclose dset)
	   (h5sclose shape)
	   (h5tclose mtype)
	   (h5tclose ftype)
	   (cffi:foreign-free wdata))

      (h5fclose file)
      (h5pclose fapl)))

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
	 (file (prog2
		   (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    
    (unwind-protect

	 (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
		(shape (h5dget-space dset)) 
		(mtype (h5tcopy +H5T-C-S1+))
		(rdata (prog2
			   (h5tset-size mtype +H5T-VARIABLE+)
			   (cffi:foreign-alloc '(:pointer :char)
					       :initial-element +NULL+
					       :count *DIM0*))))

	   (h5dread dset mtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)

	   (dotimes (i *DIM0*)
	     (format t "~a~%"
		     (cffi:foreign-string-to-lisp
		      ;; rdata is an array of pointers
		      ;; mem-aref gives us a pointer to the i-th element
		      ;; mem-ref de-references this element and
		      ;; gives us the pointer which repesents the string
		      (cffi:mem-ref (cffi:mem-aptr rdata '(:pointer :char) i)
				    '(:pointer :char)))))

	   ;;; reclaim the space allocated for individual strings
	   (h5dvlen-reclaim mtype shape +H5P-DEFAULT+ rdata)
	   ;;; still need to free the pointer array
	   (cffi:foreign-free rdata)
	   (h5tclose mtype)
	   (h5sclose shape)
	   (h5dclose dset))

      (h5fclose file)
      (h5pclose fapl))))

#+sbcl(sb-ext:quit)
