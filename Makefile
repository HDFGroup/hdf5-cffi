SBCL = /usr/local/bin/sbcl

hdf5-cffi-sbcl:
	$(SBCL) --load "make-hdf5.lisp"

test-sbcl: hdf5-cffi-sbcl
	ln -s examples/groups/h5ex_g_iterate.h5 h5ex_g_iterate.h5
	ln -s examples/groups/h5ex_g_traverse.h5 h5ex_g_traverse.h5
	ln -s examples/groups/h5ex_g_visit.h5 h5ex_g_visit.h5
	$(SBCL) --load "run-hdf5-test.lisp"

clean:
	rm -f *.data *.h5 *.o
