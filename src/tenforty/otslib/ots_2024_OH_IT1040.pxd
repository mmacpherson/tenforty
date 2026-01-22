# distutils: language = c++

cdef extern from "ots_2024.cpp" namespace "OpenTaxSolver2024::taxsolve_OH_IT1040_2024":
    int main( int argc, char *argv[] )

