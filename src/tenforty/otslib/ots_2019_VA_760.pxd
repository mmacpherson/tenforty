# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2019::taxsolve_VA_760_2019":
    int main( int argc, char *argv[] )
