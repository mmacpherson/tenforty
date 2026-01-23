# distutils: language = c++

cdef extern from "ots_2021_f8959.cpp" namespace "OpenTaxSolver2021::taxsolve_f8959_2021":
    int main( int argc, char *argv[] )
