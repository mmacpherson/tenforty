# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2020::taxsolve_PA_40_2020":
    int main( int argc, char *argv[] )
