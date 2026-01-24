# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2024::taxsolve_MI_1040_2024":
    int main( int argc, char *argv[] )
