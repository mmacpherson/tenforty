# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2023::taxsolve_PA_40_2023":
    int main( int argc, char *argv[] )
