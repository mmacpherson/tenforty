# distutils: language = c++

cdef extern from "ots_2022_NY_IT201.cpp" namespace "OpenTaxSolver2022::taxsolve_NY_IT201_2022":
    int main( int argc, char *argv[] )
