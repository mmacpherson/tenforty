# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2025::taxsolve_US_1040_Sched_1_A":
    int main( int argc, char *argv[] )
