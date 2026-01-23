# distutils: language = c++

cdef extern from "ots_2020_NY_IT201.cpp" namespace "OpenTaxSolver2020::taxsolve_NY_IT201_2020":
    int main( int argc, char *argv[] )
