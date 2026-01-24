# distutils: language = c++

cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2024::taxsolve_CA_5805_2024":
    int main( int argc, char *argv[] )
