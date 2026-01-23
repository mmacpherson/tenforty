# distutils: language = c++

cdef extern from "ots_2023_CA_540.cpp" namespace "OpenTaxSolver2023::taxsolve_CA_540_2023":
    int main( int argc, char *argv[] )
