# distutils: language = c++

cdef extern from "ots_2023_NJ_1040.cpp" namespace "OpenTaxSolver2023::taxsolve_NJ_1040_2023":
    int main( int argc, char *argv[] )
