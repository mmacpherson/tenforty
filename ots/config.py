OTS_CONFIG = {
    2018: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            ("taxsolve_US_1040_2018.c", "us"),
            ("taxsolve_US_1040_Sched_C_2018.c", "us_c"),
            ("taxsolve_CA_540_2018.c", "ca"),
            ("taxsolve_MA_1_2018.c", "ma"),
            ("taxsolve_NC_D400_2018.c", "nc"),
            ("taxsolve_NJ_1040_2018.c", "nj"),
            ("taxsolve_NY_IT201_2018.c", "ny"),
            ("taxsolve_OH_IT1040_2018.c", "oh"),
            ("taxsolve_PA_40_2018.c", "pa"),
            ("taxsolve_VA_760_2018.c", "va"),
        ),
        substitutions=("FedReturnData",),
    )
}
