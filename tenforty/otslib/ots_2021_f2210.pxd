# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2021::taxsolve_f2210_2021":
    int main( int argc, char *argv[] )
