# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2020::taxsolve_US_1040_2020":
    int main( int argc, char *argv[] )
