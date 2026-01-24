# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2018::taxsolve_CA_540_2018":
    int main( int argc, char *argv[] )
