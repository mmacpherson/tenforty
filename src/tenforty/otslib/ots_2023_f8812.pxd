# distutils: language = c++

cdef extern from "ots_2023_f8812.cpp" namespace "OpenTaxSolver2023::taxsolve_f8812_2023":
    int main( int argc, char *argv[] )
