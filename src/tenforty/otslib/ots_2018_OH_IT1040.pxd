# distutils: language = c++

cdef extern from "ots_2018_OH_IT1040.cpp" namespace "OpenTaxSolver2018::taxsolve_OH_IT1040_2018":
    int main( int argc, char *argv[] )

