# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2025::taxsolve_VA_760_2025":
    int main( int argc, char *argv[] )
