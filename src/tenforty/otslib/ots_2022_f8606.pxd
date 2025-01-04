# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2022::taxsolve_f8606":
    int main( int argc, char *argv[] )
