# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2023::taxsolve_f2210_2023":
    int main( int argc, char *argv[] )
