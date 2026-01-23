# distutils: language = c++

cdef extern from "ots_2024_NY_IT201.cpp" namespace "OpenTaxSolver2024::taxsolve_NY_IT201_2024":
    int main( int argc, char *argv[] )
