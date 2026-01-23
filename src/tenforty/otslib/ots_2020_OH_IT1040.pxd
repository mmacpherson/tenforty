# distutils: language = c++

cdef extern from "ots_2020_OH_IT1040.cpp" namespace "OpenTaxSolver2020::taxsolve_OH_IT1040_2020":
    int main( int argc, char *argv[] )
