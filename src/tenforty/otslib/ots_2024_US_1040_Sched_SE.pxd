# distutils: language = c++

cdef extern from "ots_2024.cpp" namespace "OpenTaxSolver2024::taxsolve_US_1040_Sched_SE_2024":
    int main( int argc, char *argv[] )

