"""Dict of obsolete vars and their new names for the input validator.

This is used by the input_validator module to find any obsolete variables in the 
input file (which have since been renamed in the current version of the source).
If the input validator finds an obsolete var, it can then suggest the new name
of that variable, based on this dictionary. This should make migration of old
input files easier, and variable renaming less painful.

Each key is an obsolete var, the value is either the new var name or None if the
var is deprecated.
"""
OBS_VARS = {
    "snull": "i_single_null",
    "tfno":	"n_tf",
    "itfsup": "i_tf_sup",
    "r_tf_inleg_mid": "r_tf_inboard_mid",
    "rtot":	"r_tf_outboard_mid",
    "tfareain":	"a_tf_inboard",
    "r_tf_inleg_in": "r_tf_inleg_in",
    "r_tf_inleg_out": "r_tf_inleg_out",
    "awpc":	"a_tf_wp",
    "sigttf" :"sig_tf_t", 
    "sigtcon":"sig_tf_t", 
    "sigrtf" :"sig_tf_r", 
    "sigrcon":"sig_tf_r", 
    "sigvert":"sig_tf_z", 
    "sig_vmises_case":"sig_tf_vmises", 
    "sig_vmises_cond":"sig_tf_vmises", 
    "sig_tresca_case":"sig_tf_treca", 
    "sig_tresca_cond":"sig_tf_treca",
    "sigver":"None",
    "sigrad":"None",
    "poisson":"poisson_steel",
    "eywp":"eyoung_winding",
    "eyins":"eyoung_ins",
    "eystl":"eyoung_steel",
    "isumattf":"i_tf_sc_mat",
    "turnstf":"n_tf_turn",
    "awptf":"a_tf_wp",
    "thkcas":"dr_tf_case_in",
    "tinstf":"dr_tf_ins",
    "casthi_fraction":"f_tf_case_out",
    "casthi":"dr_tf_case_out",
    "eyoung_reinforced_al":"eyoung_nibron",
    "thkwp":"dr_tf_wp",
    "leni":"t_cable",
    "leno":"t_turn",
    "conductor_width":"t_conductor"
}
