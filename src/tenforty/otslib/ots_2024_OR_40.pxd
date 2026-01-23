# distutils: language = c++

cdef extern from "ots_2024_OR_40.cpp" namespace "OpenTaxSolver2024::taxsolve_OR_40_2024":
    int main( int argc, char *argv[] )

