(in-package :hdf5)

;;; Helper code to determine library paths from
;;; environment at compile time. This assumes the
;;; same module conventions as C compilation.

(eval-when (:compile-toplevel :execute)
  ;; Helper functions
  (defun get-env-value (name &optional default)
    (or #+CMU (cdr (assoc name ext:*environment-list* :test #'string=))
        #+Allegro (sys:getenv name)
        #+CCL (ccl::getenv name)
        #+CLISP (ext:getenv name)
        #+ECL (si:getenv name)
        #+SBCL (sb-unix::posix-getenv name)
        #+LISPWORKS (lispworks:environment-variable name)
        default))
  (defun directory-pathname (name)
    (if name (pathname (concatenate 'string name "/"))))
  (defun split-path (path)
    (loop
       for rpath = (concatenate 'string path ":")
       then (subseq rpath (1+ index))
       for index = (position #\: rpath)
       for item = (subseq rpath 0 index)
       while index
       unless (string= item "") collect (directory-pathname item)))
  (defun find-in-path (path &rest pathnames)
    (dolist (prefix path)
      (dolist (target pathnames)
        (let ((truename (probe-file (merge-pathnames target prefix))))
          (when truename
            (return-from find-in-path truename)))))
    (error "Could not find any of ~S in path ~S" pathnames path))
  ;; Find mpicc in path
  (defparameter *h5cc-name*
    (loop
       for item in (split-path (get-env-value "PATH" ""))
       for name = (merge-pathnames #P"h5cc" item)
       for truename = (probe-file name)
       when truename return (list truename)))
  ;; Search path for includes and libs
  (defparameter *hdf5-include-path*
    (let ((h5inc (get-env-value "HDF5_INCLUDE")))
      (append
       ;; HDF5_INCLUDE has precedence
       (if h5inc (list (directory-pathname h5inc)))
       ;; Then try a path derived from h5cc if found.
       (mapcar (lambda (x) (merge-pathnames #P"../include/" x)) *h5cc-name*)
       ;; Hard-coded paths
       (list #P"/usr/include/hdf5/"))))
  (defparameter *hdf5-lib-path*
    (let (;(truename (or *compile-file-truename* *load-truename*))
          (h5lib (get-env-value "HDF5_LIB")))
      (append
       ;; Use the stub library if built
       ;;(if truename (list (merge-pathnames #P"mpich2-stub/" truename)))
       ;; Use HDF5_LIB if defined
       (if h5lib (list (directory-pathname h5lib)))
       ;; Try deriving a path from mpicc name
       (mapcar (lambda (x) (merge-pathnames #P"../lib/" x)) *h5cc-name*)
       ;; Look in LD_LIBRARY_PATH
       (split-path (get-env-value "LD_LIBRARY_PATH" ""))
       ;; Hard-coded paths
       (list #P"/usr/lib/hdf5/lib/")))))

;;;
;;; Actual configuration, with defaults guessed from the environment.
;;;

(defvar *hdf5-header-file*
  #.(find-in-path *hdf5-include-path* #P"hdf5.h"))

(defvar *hdf5-shared-library*
  #.(find-in-path *hdf5-lib-path*
                  #P"libhdf5.so"
                  #P"libhdf5.dylib"))

(defvar *hdf5-cc-flags* "-lhdf5")

(defparameter +NULL+ (cffi:null-pointer))

(pushnew :hdf5 *features*)

(defun load-hdf5-foreign-libraries ()
  (cffi:load-foreign-library *hdf5-shared-library*))
