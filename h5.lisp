;;;; Most API types related types are defined in H5public.h.
;;;; The defintions are driven by the H5_SIZEOF_* macros in H5pubconf.h
;;;; Check your installation and modify the definitions accordingly!

(in-package #:hdf5-cffi)

(defctype herr-t :int)
(defctype hbool-t :unsigned-int)
(defctype htri-t :int)

;;; This is our best shot at size_t and ssize_t.

(ecase +SIZE-OF-SIZE-T+
  (4 (defctype size-t :uint32))
  (8 (defctype size-t :uint64)))

(if (eq +SIZE-OF-SIZE-T+ +SIZE-OF-INT+)
    (defctype ssize-t :int)
    (if (eq +SIZE-OF-SIZE-T+ +SIZE-OF-LONG+)
	(defctype ssize-t :long)
	(if (eq +SIZE-OF-SIZE-T+ +SIZE-OF-LONG-LONG+)
	    (defctype ssize-t :long-long)
	    (error "Nothing appropriate for ssize_t found."))))

(if (>= +SIZE-OF-LONG-LONG+ 8)
    (progn
      (defctype hsize-t :unsigned-long-long)
      (defctype hssize-t :long-long)
      (defconstant +SIZE-OF-HSIZE-T+ (foreign-type-size :unsigned-long-long))
      (defconstant +SIZE-OF-HSSIZE-T+ (foreign-type-size :long-long)))
    (error "Nothing appropriate for hsize_t and hssize_t found."))

(defctype haddr-t :uint64)
(defconstant +SIZE-OF-HADDR-T+ (foreign-type-size :uint64))
(defconstant +HADDR-UNDEF+ (1- (ash 1 64)))
(defconstant +HADDR-MAX+ (1- +HADDR-UNDEF+))

;;; most important property list ;-)

(defconstant +H5P-DEFAULT+ 0)

;;; make sure the HDF5 library is initialized
(let ((ierr (foreign-funcall "H5open" herr-t)))
  (if (or (not ierr) (< ierr 0))
      (error "H5open failed.")))

;;; get version information

(defvar +H5-VERS-MAJOR+   0)
(defvar +H5-VERS-MINOR+   0)
(defvar +H5-VERS-RELEASE+ 0)

(with-foreign-object (nums :uint 3)
  (let ((ierr (foreign-funcall "H5get_libversion"
			       (:pointer :uint) (mem-aptr nums :uint 0)
			       (:pointer :uint) (mem-aptr nums :uint 1)
			       (:pointer :uint) (mem-aptr nums :uint 2)
			       herr-t)))
    (if (or (not ierr) (< ierr 0))
	(error "H5get_libversion failed.")
	(setq +H5-VERS-MAJOR+   (mem-aref nums :uint 0)
	      +H5-VERS-MINOR+   (mem-aref nums :uint 1)
	      +H5-VERS-RELEASE+ (mem-aref nums :uint 2)))))

(defcenum h5-iter-order-t
  (:H5-ITER-UNKNOWN -1)
  :H5-ITER-INC
  :H5-ITER-DEC
  :H5-ITER-NATIVE
  :H5-ITER-N)

(defconstant +H5-ITER-ERROR+ -1)
(defconstant +H5-ITER-CONT    0)
(defconstant +H5-ITER-STOP    1)

(defcenum h5-index-t
  (:H5-INDEX-UNKNOWN -1)
  :H5-INDEX-NAME
  :H5-INDEX-CRT-ORDER
  :H5-INDEX-N)

(defcstruct h5-ih-info-t
    (index-size hsize-t)
    (heap-size hsize-t))

;;; functions

(defcfun "H5close" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Close")

(defcfun "H5dont-atexit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-DontAtExit")

(defcfun "H5free-memory" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-FreeMemory"
  (buf :pointer))

(defcfun "H5garbage_collect" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-GarbageCollect")

(defcfun "H5get_libversion" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Version"
  (majnum (:pointer :unsigned-int))
  (minnum (:pointer :unsigned-int))
  (relnum (:pointer :unsigned-int)))

(defcfun "H5open" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open")

(defcfun "H5set_free_list_limits" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-SetFreeListLimits"
  (reg-global-lim :int)
  (reg-list-lim :int)
  (arr-global-lim :int)
  (arr-list-lim :int)
  (blk-global-lim :int)
  (blk-list-lim :int))
