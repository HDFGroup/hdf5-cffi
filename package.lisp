;;;; package.lisp

(defpackage #:hdf5-cffi
  (:use #:cl #:cffi)
  (:export
   
   ;; h5
   
   :haddr-t
   :hbool-t
   :herr-t
   :hsize-t
   :hssize-t
   :htri-t
   :size-t
   :ssize-t
      
   :+HADDR-UNDEF+
   :+HADDR-MAX+
   :+H5P-DEFAULT+
   :+H5-VERS-MAJOR+
   :+H5-VERS-MINOR+
   :+H5-VERS-RELEASE+

   :h5close
   :h5dont-atexit
   :h5free-memory
   :h5garbage-collect
   :h5get-libversion
   :h5open
   :h5set-free-list-limits

   ;; h5i

   :hid-t
   :h5i-type-t

   :+H5I-INVALID-HID+

   :h5iget-file-id
   :h5iget-name
   :h5iget-type
   :h5iis-valid
   :h5inmembers

   ;; h5f

   :h5f-intent-flags
   :h5f-scope-t

   :h5fclose
   :h5fcreate
   :h5fflush
   :h5fget-access-plist
   :h5fcreate-plist
   :h5fget-filesize
   :h5fget-intent
   :h5fget-name
   :h5fmount
   :h5fopen
   :h5freopen
   :h5funmount))

