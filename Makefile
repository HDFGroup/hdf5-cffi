SBCL = /usr/local/bin/sbcl

hdf5-cffi-sbcl:
	$(SBCL) --load "make-hdf5.lisp"

test-sbcl: hdf5-cffi-sbcl
	$(SBCL) --load "run-hdf5-test.lisp"
