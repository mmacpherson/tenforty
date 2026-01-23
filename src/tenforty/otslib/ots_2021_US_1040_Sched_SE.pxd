# distutils: language = c++

cdef extern from "ots_2021_US_1040_Sched_SE.cpp" namespace "OpenTaxSolver2021::taxsolve_US_1040_Sched_SE_2021":
    int main( int argc, char *argv[] )
