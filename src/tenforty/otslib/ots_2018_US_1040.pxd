# distutils: language = c++

cdef extern from "ots_2018_US_1040.cpp" namespace "OpenTaxSolver2018::taxsolve_US_1040_2018":
    int main( int argc, char *argv[] )

