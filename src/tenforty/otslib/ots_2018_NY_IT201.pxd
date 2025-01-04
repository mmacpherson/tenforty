# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2018::taxsolve_NY_IT201_2018":
    int main( int argc, char *argv[] )
