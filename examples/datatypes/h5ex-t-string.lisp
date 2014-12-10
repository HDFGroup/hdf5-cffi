;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write string datatypes
;;; to a dataset.  The program first writes strings to a
;;; dataset with a dataspace of DIM0, then closes the file.
;;; Next, it reopens the file, reads back the data, and
;;; outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_string.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE*    "h5ex_t_string.h5")
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 4)
(defparameter *SDIM* 8)


(defun create-simple-dataspace (dims-seq)
  (let* ((dims (cffi:foreign-alloc 'hsize-t :count (list-length dims-seq)
                                   :initial-contents dims-seq))
         ;; Create dataspace. Setting maximum size to NULL sets the
         ;; maximum size to be the current size.
         (space (h5screate-simple (list-length dims-seq) dims +NULL+)))
    (cffi:foreign-free dims)
    space))


;;; Create file and memory datatypes.  For this example we will save
;;; the strings as FORTRAN strings, therefore they do not need space
;;; for the null terminator in the file.

(defun create-filetype ()
  (let ((result (h5tcopy +H5T-FORTRAN-S1+)))
    (h5tset-size result (1- *SDIM*))
    result))


(defun create-memtype ()
  (let ((result (h5tcopy +H5T-C-S1+)))
    (h5tset-size result *SDIM*)
    result))


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (cffi:foreign-string-alloc
                      "Parting is such sweet   sorrow. "))
              (space (create-simple-dataspace (list *DIM0*)))
              (filetype (create-filetype))
              (memtype (create-memtype))
              ;; Create the dataset and write the array data to it.
              (dset (h5dcreate2 file *DATASET* filetype space
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
         (h5dwrite dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ wdata)
         ;; Close and release resources.
         (h5tclose memtype)
         (h5tclose filetype)
         (h5dclose dset)
         (h5sclose space)
         (cffi:foreign-string-free wdata))
    (h5fclose file)
    (h5pclose fapl)))

;; Now we begin the read section of this example.  Here we assume
;; the dataset has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              ;; Get the datatype and its size.
              (filetype (h5dget-type dset))
              (sdim (1+ (h5tget-size filetype)))
              (memtype (create-memtype))
              (space (h5dget-space dset)))
         
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
	     ;; Allocate space for character data.
	     (cffi:with-foreign-object (rdata :char (* dims[0] sdim))
	       ;; Read the data.
	       (h5dread dset memtype +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+ rdata)
	       ;; Output the data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~d]: ~a~%" *DATASET* i
                         (cffi:foreign-string-to-lisp
                          (cffi:mem-aptr rdata :char (* i sdim))))))))
         ;; Close and release resources.
         (h5sclose space)
         (h5tclose memtype)
         (h5tclose filetype)
         (h5dclose dset))
    (h5fclose file)
    (h5pclose fapl)))

#+sbcl(sb-ext:quit)
