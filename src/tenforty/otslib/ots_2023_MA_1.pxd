# distutils: language = c++

cdef extern from "ots_2023_MA_1.cpp" namespace "OpenTaxSolver2023::taxsolve_MA_1_2023":
    int main( int argc, char *argv[] )
