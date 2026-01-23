# distutils: language = c++

cdef extern from "ots_2022_f8959.cpp" namespace "OpenTaxSolver2022::taxsolve_f8959_2022":
    int main( int argc, char *argv[] )
