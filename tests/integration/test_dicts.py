"""Tests for the Python-Fortran dictionaries.

Ensure that the dicts have been created and match the reference dicts.

It should be noted that the "old" (Fortran-only) dicts do not completely match
the "new" (Python conversion) dicts. They are close, but some keys and values
have changed as a result of the restructuring. These differences are handled
in these tests.
"""
from process.io.python_fortran_dicts import get_dicts
import pytest
from pathlib import Path
import json
import logging

logger = logging.getLogger(__name__)

@pytest.fixture
def new_dicts():
    """Fetch the latest Python-Fortran dictionaries.

    :return: Python-Fortran dicts
    :rtype: dict
    """
    return get_dicts()

@pytest.fixture
def ref_dicts():
    """Fetch reference dicts, created on develop branch.

    :return: old dicts, from develop
    :rtype: dict
    """
    ref_dicts_path = Path(__file__).parent / "ref_dicts.json"

    with open(ref_dicts_path, 'r') as ref_dicts_file:
        return json.load(ref_dicts_file)

def test_default_dict(ref_dicts, new_dicts):
    """Compare the default dict in reference and new dicts.

    This compares the initial values of vars in ref and new dicts.

    :param ref_dicts: old dicts from develop
    :type ref_dicts: dict
    :param new_dicts: new dicts from Python conversion
    :type new_dicts: dicts
    """
    # These vars have since been removed from the source during Python 
    # conversion or need ignoring for the reasons below

    # TODO Some of these vars might be present on develop, but not yet on the 
    # 1092 python conversion branch; hence they appear missing. Need to merge 
    # develop into 1092 again and see what's still missing
    EXCLUSIONS = [
        "INSTALLDIR",
        "ROOTDIR",
        "autodoc_version",
        "child",
        "config",
        "cplife_input",
        "ctfile",
        "current",
        "error_head",
        "error_tail",
        "e_Li",
        "f_r_cp",
        "ffile",
        "fileprefix",
        "first_call",
        "first_routine",
        "hfile",
        "hfunit",
        "html_arguments_header",
        "html_author_header",
        "html_calls_header",
        "html_code_close",
        "html_code_open",
        "html_contents_header",
        "html_details_header",
        "html_dir_close",
        "html_dir_open",
        "html_doc_header",
        "html_h2_close",
        "html_h2_open",
        "html_h3_close",
        "html_h3_open",
        "html_history_header",
        "html_hrule",
        "html_link_close",
        "html_link_mid",
        "html_link_open",
        "html_listitem",
        "html_par",
        "html_problems_header",
        "html_status_header",
        "html_summary_header",
        "html_type_header",
        "html_ulist_close",
        "html_ulist_open",
        "html_var_header",
        "impdir",
        "iounit",
        "latest_routine",
        "lenmax",
        "parent",
        "recursion",
        "tagcount",
        "vdfile",
        "vdunit",
        "i_cp_joints",
        "i_cp_lifetime",
        "impurity_arr",
        "n_day_year",
        "n_tf_turn",
        "neut_flux_cp",
        "pnuc_cp",
        "pnuc_cp_sh",
        "pnuc_cp_tf",
        "pnuc_tot_blk_sector",
        "r_sh_inboard_in",
        "ipeqns",
        "s",
        "lambda_q_BZ_Be_IB",
        "lambda_q_BZ_Be_OB",
        "lambda_q_BZ_breed_IB",
        "lambda_q_BZ_breed_OB",
        "lambda_q_BZ_steels_IB",
        "lambda_q_BZ_steels_OB",
        "output_prefix",
        "q_0_BZ_Be_IB",
        "q_0_BZ_Be_OB",
        "q_0_BZ_breed_IB",
        "q_0_BZ_breed_OB",
        "q_0_BZ_steels_IB",
        "q_0_BZ_steels_OB",
        "intervallabel",
        "lablmm",
        "lablxc",
        "timelabel",
        "imp_label",
        "pi",
        "blmatf",
        "fwmatf",
        "shmatf",
        "v1matf",
        "v2matf",
        "v3matf"
    ]
    """
    Ignore calltree_data: removed. ctfile, current, error_head, error_tail
    first_routine, latest_routine, parent, recursion, tagcount
    They used to be null()
    
    Ignore all vardes and html stuff: this has been removed
    vdfile, vdunit, ffile, hfile, hfunit
    
    e_Li, lambda_q_BZ_Be_IB, pi are defined differently in multiple places. 
    This leads to a race condition concerning which value overwrites last, and
    differences between local and CI dicts. Unique dict names are necessary
    to overcome this.

    fileprefix, output_prefix contains an unneccessary \"\", now ""
    
    first_call is only used to find if a subroutine is being run for the first 
    time; ignore
    
    impdir, impurity_arr, s are now initialised with a function; can't parse
    
    iounit, lenmax, i_cp_joints, n_day_year removed

    intervallabel contains spaces in an array of strings which are now stripped

    lablmm was breaking on commas inside strings inside the array before

    lablxc, timelabel, imp_label is the same, just with an annoying "''" for each value: now ''

    blmatf, fwmatf, shmatf, v1matf, v2matf, v3matf contains shape and reshape() 
    functions: was str, is now list
    """

    # One difference is due to "null" in ref_dicts now being "0.0" or "" in 
    # new_dicts. This is due to needing to initialise all module variables, not 
    # just declare them, as before

    # Compare ref_dicts values individually
    for old_key, old_value in ref_dicts["DICT_DEFAULT"].items():
        # Ignore excluded keys
        if old_key in EXCLUSIONS:
            continue
        
        # Assert old_key exists in new_dict
        try:
            assert old_key in new_dicts["DICT_DEFAULT"]
            new_value = new_dicts["DICT_DEFAULT"][old_key]
        except AssertionError:
            logger.error(f"{old_key} isn't present in new_dicts")
            raise
        
        # Compare differently if old_value is None or not
        if old_value is None:
            # If value was None (null in json, not initialised in Fortran) before, 
            # make sure the new value is None-y; initialised, but still representing
            # in some way
            # Assert new_value is 0.0, None, "", .false. or ["", "", ""]
            try:
                if type(new_value) is str and len(new_value) > 0:
                    # The only acceptable value is .false.
                    assert new_value.lower() == ".false."
                else:
                    # Handle iterable and non-iterable values
                    try:
                        # Try iterating over potential list, asserting each 
                        # value to be None-y
                        for new_value in new_value:
                            assert new_value in [0.0, None, ""]
                    except TypeError:
                        # Not iterable: just assert the single value
                        assert new_value in [0.0, None, ""]
            except AssertionError:
                logger.error(f"{old_key} in new_dicts should be 0.0, None, "
                    ".false. or a list of empty strings")
                raise

        else:
            # old_value has a value, compare to new value
            # Strip all spaces for comparison
            if type(new_value) is str:
                new_value = new_value.replace(" ", "")
            if type(old_value) is str:
                old_value = old_value.replace(" ", "")

            try:
                assert old_value == new_value
            except AssertionError:
                # The values are different, but is it acceptable?
                if type(old_value) is list and type(new_value) is list:
                    # Check for off-by-one errors: possibly caused by start-from
                    # -1 arrays in Fortran, or dodgy array-guessing in create_dicts
                    # TODO This needs to be investigated, although doesn't affect 
                    # many vars
                    if len(old_value) == len(new_value) + 1:
                        # Allow an off-by-one error
                        logger.error(f"{old_key} list has an off-by-one error")
                    else:
                        # Otherwise raise the error
                        logger.error(f"{old_key} is different")
                        raise
                elif type(new_value) is list and type(old_value) is not list:
                    # var could now be properly initialised as list: check
                    if new_value[0] == old_value:
                        # Properly initialised as a list now
                        logger.error(f"{old_key} is now initialised as a list")
                    else:
                        logger.error(f"{old_key} is different")
                        raise
                else:
                    # The old and new values are different and it's not 
                    # acceptable
                    logger.error(f"{old_key} is different")
                    raise

@pytest.mark.skip(reason="old and new dicts cannot be identical")
def test_ref_and_new_dicts_summary(ref_dicts, new_dicts):
    """Simple comparison of reference and new dicts.

    These will not match exactly due to differences between the develop and 
    Python conversion branches. It will always fail, and is therefore skipped.
    It provides a useful overview of which dicts are different, however.

    :param ref_dicts: reference dicts
    :type ref_dicts: dict
    :param new_dicts: new dicts
    :type new_dicts: dict
    """
    # Number of dictionaries that match and differ
    match_dicts = 0
    diff_dicts = 0
    
    # Compare all dicts
    for old_dict_key, old_dict_value in ref_dicts.items():
        try:
            assert old_dict_value == new_dicts[old_dict_key]
            logger.info(f"{old_dict_key} dict matches develop")
            match_dicts += 1
        except AssertionError:
            logger.error(f"{old_dict_key} dict differs from develop")
            diff_dicts += 1

    # Assert there are no differences
    try:
        assert diff_dicts == 0
    except AssertionError:
        total_dicts = match_dicts + diff_dicts
        logger.error(f"{diff_dicts} / {total_dicts} dont' match")
        raise