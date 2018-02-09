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
;;; to an attribute.  The program first writes strings to an
;;; attribute with a dataspace of DIM0, then closes the file.
;;; Next, it reopens the file, reads back the data, and
;;; outputs it to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_stringatt.c





(in-package :hdf5)

(defparameter *FILE*    (namestring (merge-pathnames "h5ex_t_string.h5" *load-pathname*)))
(defparameter *DATASET* "DS1")
(defparameter *ATTRIBUTE* "A1")
(defparameter *DIM0* 4)
(defparameter *SDIM* 8)


;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let* ((wdata (cffi:foreign-string-alloc
                      "Parting is such sweet   sorrow. "))
              ;; Create file and memory datatypes.  For this example we will save
              ;; the strings as FORTRAN strings, therefore they do not need space
              ;; for the null terminator in the file.
              (filetype (h5ex:create-f-string-type (1- *SDIM*)))
              (memtype (h5ex:create-c-string-type *SDIM*))
              ;; Create dataset with a null dataspace.
              (dspace (h5ex:create-null-dataspace))
              (dset (h5dcreate2 file *DATASET* +H5T-STD-I32LE+ dspace
                                +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+))
              ;; Create the attribute and write the string data to it.
              (aspace (h5ex:create-simple-dataspace `(,*DIM0*)))
              (attr (h5acreate2 dset *ATTRIBUTE* filetype aspace +H5P-DEFAULT+
                                +H5P-DEFAULT+)))
         (h5awrite attr memtype wdata)
         ;; Close and release resources.
         (h5ex:close-handles (list filetype memtype dspace dset aspace attr))
         (cffi:foreign-string-free wdata))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the attribute has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (attr (h5aopen dset *ATTRIBUTE* +H5P-DEFAULT+))
              ;; Get the datatype and its size.
              (filetype (h5aget-type attr))
              (sdim (1+ (h5tget-size filetype)))
              (memtype (h5ex:create-c-string-type sdim))
              (space (h5aget-space attr)))
         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
	     ;; Allocate space for character data.
	     (cffi:with-foreign-object (rdata :char (* dims[0] sdim))
	       ;; Read the data.
	       (h5aread attr memtype rdata)
	       ;; Output the data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~d]: ~a~%" *ATTRIBUTE* i
                         (cffi:foreign-string-to-lisp
                          (cffi:mem-aptr rdata :char (* i sdim))))))))
         ;; Close and release resources.
         (h5ex:close-handles (list space memtype filetype attr dset)))
    (h5ex:close-handles (list file fapl))))
