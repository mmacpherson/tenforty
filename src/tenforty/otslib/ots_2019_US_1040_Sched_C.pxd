# distutils: language = c++

cdef extern from "ots_2019_US_1040_Sched_C.cpp" namespace "OpenTaxSolver2019::taxsolve_US_1040_Sched_C_2019":
    int main( int argc, char *argv[] )

