# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2023::taxsolve_US_1040_Sched_E_brokerage_royalties_2023":
    int main( int argc, char *argv[] )
