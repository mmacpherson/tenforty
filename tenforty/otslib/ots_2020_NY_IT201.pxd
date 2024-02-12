# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2020::taxsolve_NY_IT201_2020":
    int main( int argc, char *argv[] )
