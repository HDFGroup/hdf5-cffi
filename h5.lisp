;;;; Most API types related types are defined in H5public.h.
;;;; The defintions are driven by the H5_SIZEOF_* macros in H5pubconf.h
;;;; Check your installation and modify the definitions accordingly!

(in-package #:hdf5-cffi)

(defctype herr-t :int)
(defctype hbool-t :unsigned-int)
(defctype htri-t :int)

;;; This is our best shot at size_t and ssize_t.

(ecase (foreign-type-size :pointer)
  (4 (defctype size-t :uint32))
  (8 (defctype size-t :uint64)))

(if (eq (foreign-type-size :pointer)
        (foreign-type-size :int))
    (defctype ssize-t :int)
    (if (eq (foreign-type-size :pointer)
	    (foreign-type-size :long))
	(defctype ssize-t :long)
	(if (eq (foreign-type-size :pointer)
		(foreign-type-size :long-long))
	    (defctype ssize-t :long-long)
	    (error "Nothing appropriate for ssize_t found."))))

(if (>= (foreign-type-size :long-long) 8)
    (progn
      (defctype hsize-t :unsigned-long-long)
      (defctype hssize-t :long-long))
    (error "Nothing appropriate for hsize_t and hssize_t found."))

(defctype haddr-t :uint64)
(defconstant +HADDR-UNDEF+ (1- (ash 1 64)))
(defconstant +HADDR-MAX+ (1- +HADDR-UNDEF+))

(defconstant +H5P-DEFAULT+ 0)

