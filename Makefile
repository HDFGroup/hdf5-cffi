###############################################################################
#
#
# look for HDF5 installation; generate specialized version of grovel file
#
# set CC = h5cc so that cffi-grovel can compile h5-grovel correctly

#CL = $(shell which ccl)
CL = $(shell which sbcl)

hdf5-cffi:
	CC=h5cc $(CL) --load "make-hdf5.lisp"

test: hdf5-cffi
	ln -s examples/groups/h5ex_g_iterate.h5 h5ex_g_iterate.h5
	ln -s examples/groups/h5ex_g_traverse.h5 h5ex_g_traverse.h5
	ln -s examples/groups/h5ex_g_visit.h5 h5ex_g_visit.h5
	$(CL) --load "run-hdf5-test.lisp"

clean:
	rm -f *.data *.h5 *.o
