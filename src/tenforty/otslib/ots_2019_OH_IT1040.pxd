# distutils: language = c++

cdef extern from "ots_2019_OH_IT1040.cpp" namespace "OpenTaxSolver2019::taxsolve_OH_IT1040_2019":
    int main( int argc, char *argv[] )

