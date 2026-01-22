# distutils: language = c++

cdef extern from "ots_2018.cpp" namespace "OpenTaxSolver2018::taxsolve_MA_1_2018":
    int main( int argc, char *argv[] )
