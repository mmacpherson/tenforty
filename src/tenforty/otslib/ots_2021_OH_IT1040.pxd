# distutils: language = c++

cdef extern from "ots_2021_OH_IT1040.cpp" namespace "OpenTaxSolver2021::taxsolve_OH_IT1040_2021":
    int main( int argc, char *argv[] )

