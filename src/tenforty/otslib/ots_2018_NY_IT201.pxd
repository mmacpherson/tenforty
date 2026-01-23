# distutils: language = c++

cdef extern from "ots_2018_NY_IT201.cpp" namespace "OpenTaxSolver2018::taxsolve_NY_IT201_2018":
    int main( int argc, char *argv[] )
