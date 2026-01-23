# distutils: language = c++

cdef extern from "ots_2022_OH_IT1040.cpp" namespace "OpenTaxSolver2022::taxsolve_OH_IT1040_2022":
    int main( int argc, char *argv[] )

