###############################################################################
# Set this to match your local HDF5 installation!

H5CC = $(shell which h5cc)

###############################################################################

CC = $(H5CC)
LD_LIBRARY_PATH=$(shell echo $(H5CC) | sed -e 's/bin\/h5cc/lib/g')
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

all: hdf5-cffi-sbcl clean
