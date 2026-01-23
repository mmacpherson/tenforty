# distutils: language = c++

cdef extern from "ots_2021_NY_IT201.cpp" namespace "OpenTaxSolver2021::taxsolve_NY_IT201_2021":
    int main( int argc, char *argv[] )
