# distutils: language = c++

cdef extern from "ots_2021.cpp" namespace "OpenTaxSolver2021::taxsolve_CA_5805_2021":
    int main( int argc, char *argv[] )
