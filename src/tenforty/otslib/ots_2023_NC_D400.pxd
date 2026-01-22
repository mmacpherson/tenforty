# distutils: language = c++

cdef extern from "ots_2023.cpp" namespace "OpenTaxSolver2023::taxsolve_NC_D400_2023":
    int main( int argc, char *argv[] )
