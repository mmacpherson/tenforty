# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2021::taxsolve_US_1040_Sched_C_2021":
    int main( int argc, char *argv[] )
