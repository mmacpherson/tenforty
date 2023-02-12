# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2022::taxsolve_MA_1_2022":
    int main( int argc, char *argv[] )
