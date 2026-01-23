# distutils: language = c++

cdef extern from "ots_2019_NJ_1040.cpp" namespace "OpenTaxSolver2019::taxsolve_NJ_1040_2019":
    int main( int argc, char *argv[] )
