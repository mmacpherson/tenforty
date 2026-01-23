# distutils: language = c++

cdef extern from "ots_2020_US_1040_Sched_C.cpp" namespace "OpenTaxSolver2020::taxsolve_US_1040_Sched_C_2020":
    int main( int argc, char *argv[] )

