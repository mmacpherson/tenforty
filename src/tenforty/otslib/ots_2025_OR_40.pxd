# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2025::taxsolve_OR_40_2025":
    int main( int argc, char *argv[] )
