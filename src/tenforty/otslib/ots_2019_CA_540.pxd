# distutils: language = c++

cdef extern from "ots_2019_CA_540.cpp" namespace "OpenTaxSolver2019::taxsolve_CA_540_2019":
    int main( int argc, char *argv[] )

