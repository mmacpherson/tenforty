OTS_CONFIG = {
    2018: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            ("taxsolve_US_1040_2018.c", "_us"),
            ("taxsolve_US_1040_Sched_C_2018.c", "_us_c"),
            ("taxsolve_CA_540_2018.c", "_ca"),
            ("taxsolve_MA_1_2018.c", "_ma"),
            ("taxsolve_NC_D400_2018.c", "_nc"),
            ("taxsolve_NJ_1040_2018.c", "_nj"),
            ("taxsolve_NY_IT201_2018.c", "_ny"),
            ("taxsolve_OH_IT1040_2018.c", "_oh"),
            ("taxsolve_PA_40_2018.c", "_pa"),
            ("taxsolve_VA_760_2018.c", "_va"),
        ),
        substitutions=("FedReturnData",),
    )
}
