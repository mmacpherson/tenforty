# distutils: language = c++

cdef extern from "ots_2019_PA_40.cpp" namespace "OpenTaxSolver2019::taxsolve_PA_40_2019":
    int main( int argc, char *argv[] )

