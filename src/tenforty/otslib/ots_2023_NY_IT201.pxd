# distutils: language = c++

cdef extern from "ots_2023_NY_IT201.cpp" namespace "OpenTaxSolver2023::taxsolve_NY_IT201_2023":
    int main( int argc, char *argv[] )

