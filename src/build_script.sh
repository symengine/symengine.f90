fypp symengine_cwrapper.fypp symengine_cwrapper.f90
gfortran -Wall -c symengine_cwrapper.f90
gfortran -Wall -c test_symengine.f90
gfortran -o test_symengine -I/usr/local/include/symengine symengine_cwrapper.o test_symengine.o -L/usr/local/lib -lsymengine -lteuchos -lstdc++ -lmpfr -lgmp -lbfd
