# distutils: language = c++

cdef extern from "ots_2024_US_1040.cpp" namespace "OpenTaxSolver2024::taxsolve_US_1040_2024":
    int main( int argc, char *argv[] )

