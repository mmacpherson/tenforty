# distutils: language = c++

cdef extern from "ots_2021_PA_40.cpp" namespace "OpenTaxSolver2021::taxsolve_PA_40_2021":
    int main( int argc, char *argv[] )
