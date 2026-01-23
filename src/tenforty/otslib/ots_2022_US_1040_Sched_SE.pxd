# distutils: language = c++

cdef extern from "ots_2022_US_1040_Sched_SE.cpp" namespace "OpenTaxSolver2022::taxsolve_US_1040_Sched_SE_2022":
    int main( int argc, char *argv[] )
