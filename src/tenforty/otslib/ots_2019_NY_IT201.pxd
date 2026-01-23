# distutils: language = c++

cdef extern from "ots_2019_NY_IT201.cpp" namespace "OpenTaxSolver2019::taxsolve_NY_IT201_2019":
    int main( int argc, char *argv[] )

