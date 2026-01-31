# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2025::taxsolve_f8812_2025":
    int main( int argc, char *argv[] )
