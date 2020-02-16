# -*- coding: utf-8 -*-
from . import helpers
from . import data
from . import ots_2018


def us_1040(form_values, year="latest"):
    """Compute US federal tax return.

    """
    return_text = helpers.generate_ots_return(form_values, data.US_1040)
    return helpers.parse_ots_return(ots_2018.us_main(return_text))


def tax_rate(adjusted_gross_income, status=1, year="latest"):
    return ots_2018.tax_rate(adjusted_gross_income, status)
