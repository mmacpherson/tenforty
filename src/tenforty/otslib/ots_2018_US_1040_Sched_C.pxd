# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2018::taxsolve_US_1040_Sched_C_2018":
    int main( int argc, char *argv[] )
