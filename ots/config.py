OTS_CONFIG = {
    2020: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            # -- federal
            ("taxsolve_US_1040_2020.c", "_us"),
            ("taxsolve_US_1040_Sched_C_2020.c", "_us_c"),
            ("taxsolve_HSA_f8889.c", "_hsa"),
            # -- state
            ("taxsolve_CA_540_2020.c", "_ca"),
            ("taxsolve_MA_1_2020.c", "_ma"),
            # ("taxsolve_NC_D400_2020.c", "_nc"),
            ("taxsolve_NJ_1040_2020.c", "_nj"),
            ("taxsolve_NY_IT201_2020.c", "_ny"),
            ("taxsolve_OH_IT1040_2020.c", "_oh"),
            ("taxsolve_PA_40_2020.c", "_pa"),
            ("taxsolve_VA_760_2020.c", "_va"),
        ),
        substitutions=("FedReturnData",),
    ),
    2019: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            # -- federal
            ("taxsolve_US_1040_2019.c", "_us"),
            ("taxsolve_US_1040_Sched_C_2019.c", "_us_c"),
            # -- state
            ("taxsolve_CA_540_2019.c", "_ca"),
            ("taxsolve_MA_1_2019.c", "_ma"),
            ("taxsolve_NC_D400_2019.c", "_nc"),
            ("taxsolve_NJ_1040_2019.c", "_nj"),
            ("taxsolve_NY_IT201_2019.c", "_ny"),
            ("taxsolve_OH_IT1040_2019.c", "_oh"),
            ("taxsolve_PA_40_2019.c", "_pa"),
            ("taxsolve_VA_760_2019.c", "_va"),
        ),
        substitutions=("FedReturnData",),
    ),
    2018: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            # -- federal
            ("taxsolve_US_1040_2018.c", "_us"),
            ("taxsolve_US_1040_Sched_C_2018.c", "_us_c"),
            # -- state
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
    ),
    2017: dict(
        sources=(
            ("taxsolve_routines.c", None),  # ie, do not rename anything
            # -- federal
            ("taxsolve_US_1040_2017.c", "_us"),
            ("taxsolve_US_1040_Sched_C_2017.c", "_us_c"),
            # -- state
            ("taxsolve_CA_540_2017.c", "_ca"),
            ("taxsolve_MA_1_2017.c", "_ma"),
            ("taxsolve_NC_D400_2017.c", "_nc"),
            ("taxsolve_NJ_1040_2017.c", "_nj"),
            ("taxsolve_NY_IT201_2017.c", "_ny"),
            ("taxsolve_OH_IT1040_2017.c", "_oh"),
            ("taxsolve_PA_40_2017.c", "_pa"),
            ("taxsolve_VA_760_2017.c", "_va"),
        ),
        substitutions=("FedReturnData",),
    ),
}
