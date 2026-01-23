# distutils: language = c++

cdef extern from "ots_2018_VA_760.cpp" namespace "OpenTaxSolver2018::taxsolve_VA_760_2018":
    int main( int argc, char *argv[] )
