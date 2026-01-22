# distutils: language = c++

cdef extern from "ots_2023.cpp" namespace "OpenTaxSolver2023::taxsolve_US_1040_2023":
    int main( int argc, char *argv[] )
