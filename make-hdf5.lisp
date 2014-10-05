#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)
#+cmu(ext:quit)
#+sbcl(sb-ext:quit)
