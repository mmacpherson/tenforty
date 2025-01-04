# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2023::taxsolve_US_1040_Sched_SE_2023":
    int main( int argc, char *argv[] )
