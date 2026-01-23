# distutils: language = c++

cdef extern from "ots_2022_NC_D400.cpp" namespace "OpenTaxSolver2022::taxsolve_NC_D400_2022":
    int main( int argc, char *argv[] )
