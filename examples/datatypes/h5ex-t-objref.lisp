;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write object references
;;; to a dataset.  The program first creates objects in the
;;; file and writes references to those objects to a dataset
;;; with a dataspace of DIM0, then closes the file.  Next, it
;;; reopens the file, dereferences the references, and outputs
;;; the names of their targets to the screen.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_objref.c

#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)
(asdf:operate 'asdf:load-op 'hdf5-examples)

(in-package :hdf5)

(defparameter *FILE*    "h5ex_t_objref.h5")
(defparameter *DATASET* "DS1")
(defparameter *DIM0* 2)

;; Create a new file using the default properties.
(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let ((space (h5ex:create-null-dataspace)))
         (h5ex:close-handles
          ;; Create a dataset with a null dataspace.
          (list (h5dcreate2 file "DS2" +H5T-STD-I32LE+ space
                            +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)
                space
                ;; Create a goup.
                (h5gcreate2 file "G1" +H5P-DEFAULT+ +H5P-DEFAULT+
                            +H5P-DEFAULT+)))

         ;; Create references to the previously created objects.  Passing -1
         ;; as space_id causes this parameter to be ignored.  Other values
         ;; besides valid dataspaces result in an error.
         (cffi:with-foreign-object (wdata 'hobj-ref-t *DIM0*)
           (let ((wdata[0] (cffi:mem-aptr wdata 'hobj-ref-t 0))
                 (wdata[1] (cffi:mem-aptr wdata 'hobj-ref-t 1)))
             (h5rcreate wdata[0] file "G1" :H5R-OBJECT -1)
             (h5rcreate wdata[1] file "DS2" :H5R-OBJECT -1))

           ;; Create the dataset and write the object references to it.
           (let* ((space (h5ex:create-simple-dataspace `(,*DIM0*)))
                  (dset (h5dcreate2 file *DATASET* +H5T-STD-REF-OBJ+ space
                                    +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
             (h5dwrite dset +H5T-STD-REF-OBJ+ +H5S-ALL+ +H5S-ALL+ +H5P-DEFAULT+
                       wdata)
             (h5ex:close-handles (list dset space)))))
    (h5ex:close-handles (list file fapl))))

;; Now we begin the read section of this example.  Here we assume
;; the dataset has the same name and rank, but can have any size.
;; Therefore we must allocate a new array to read in data dynamically.

(let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                 (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
  (unwind-protect
       (let* ((dset (h5dopen2 file *DATASET* +H5P-DEFAULT+))
              (space (h5dget-space dset)))

         (cffi:with-foreign-object (dims 'hsize-t 1)
           (h5sget-simple-extent-dims space dims +NULL+)
	   (let ((dims[0] (cffi:mem-aref dims 'hsize-t 0)))
	     ;; Allocate space for integer data.
	     (cffi:with-foreign-object (rdata 'hobj-ref-t dims[0])
	       ;; Read the data.
	       (h5dread dset +H5T-STD-REF-OBJ+ +H5S-ALL+ +H5S-ALL+
                        +H5P-DEFAULT+ rdata)
	       ;; Output the data to the screen.
               (dotimes (i dims[0])
                 (format t "~a[~a]:~%  ->" *DATASET* i)
                 (let* ((ptr (cffi:mem-aptr rdata 'hobj-ref-t i))
                        (obj (h5rdereference dset :H5R-OBJECT ptr)))
                   (cffi:with-foreign-object (objtype 'h5o-type-t 1)
                     (h5rget-obj-type2 dset :H5R-OBJECT ptr objtype)

                     (let ((type (cffi:mem-ref objtype 'h5o-type-t)))
                       ;; Print the object type and close the object.
                       (cond ((eql type :H5O-TYPE-GROUP) (format t "Group"))
                             ((eql type :H5O-TYPE-DATASET)
                              (format t "Datatset"))
                             ((eql type :H5O-TYPE-NAMED-DATATYPE)
                              (format t "Named Datatype")))

                       ;; Get the length of the name, allocate space, then
                       ;; retrieve the name.
                       (let* ((size (1+ (h5iget-name obj +NULL+ 0)))
                              (name (cffi:foreign-alloc :char :count size)))
                         (h5iget-name obj name size)
                         ;; Print the name and deallocate space for the name.
                         (format t ": ~a~%" (cffi:foreign-string-to-lisp name))
                         (cffi:foreign-free name))

                     (h5oclose obj))))))))

         ;; Close and release resources.
         (h5ex:close-handles (list space dset)))
    (h5ex:close-handles (list file fapl))))
