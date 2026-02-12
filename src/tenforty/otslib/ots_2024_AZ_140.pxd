# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2024::taxsolve_AZ_140_2024":
    int main( int argc, char *argv[] )
