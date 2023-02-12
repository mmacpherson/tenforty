# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2018::taxsolve_PA_40_2018":
    int main( int argc, char *argv[] )
