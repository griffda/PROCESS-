from genericpath import exists
from process.fortran import impurity_radiation_module
from process.fortran import error_handling


import logging
from process.fortran import constants

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


class Impurity:
    def __init__(self):
        self.outfile = constants.nout

    def initialise_imprad(self):
        """
        Initialises the impurity radiation data structure
        author: H Lux, CCFE, Culham Science Centre
        None
        This routine initialises the impurity radiation data.
        """

        errorflag = 0

        table_length = 100  # Number of temperature and Lz values in data file
        tmult = 1.0e0  # Conversion from temperatures in data file to keV
        lzmult = 1.0e0  # Conversion from Lz values in data file to W/m3

        frac = 1.0e0

        #  Hydrogen

        impurity_radiation_module.init_imp_element(
            no=1,
            label=impurity_radiation_module.imp_label[0],
            z=1,
            amass=1.01e0,
            frac=frac,
            len_tab=table_length,
            tinkev=tmult,
            lzinwm3=lzmult,
            error=errorflag,
        )

        frac = 0.0e0

        #  Helium
        impurity_radiation_module.init_imp_element(
            2,
            impurity_radiation_module.imp_label[1],
            2,
            4.003e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Beryllium
        impurity_radiation_module.init_imp_element(
            3,
            impurity_radiation_module.imp_label[2],
            4,
            9.01e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Carbon
        impurity_radiation_module.init_imp_element(
            4,
            impurity_radiation_module.imp_label[3],
            6,
            12.01e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Nitrogen
        impurity_radiation_module.init_imp_element(
            5,
            impurity_radiation_module.imp_label[4],
            7,
            14.01e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Oxygen
        impurity_radiation_module.init_imp_element(
            6,
            impurity_radiation_module.imp_label[5],
            8,
            15.999e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Neon
        impurity_radiation_module.init_imp_element(
            7,
            impurity_radiation_module.imp_label[6],
            10,
            20.18e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Silicon
        impurity_radiation_module.init_imp_element(
            8,
            impurity_radiation_module.imp_label[7],
            14,
            28.09e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Argon
        impurity_radiation_module.init_imp_element(
            9,
            impurity_radiation_module.imp_label[8],
            18,
            39.95e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Iron
        impurity_radiation_module.init_imp_element(
            10,
            impurity_radiation_module.imp_label[9],
            26,
            55.85e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Nickel
        impurity_radiation_module.init_imp_element(
            11,
            impurity_radiation_module.imp_label[10],
            28,
            58.70e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Krypton
        impurity_radiation_module.init_imp_element(
            12,
            impurity_radiation_module.imp_label[11],
            36,
            83.80e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Xenon
        impurity_radiation_module.init_imp_element(
            13,
            impurity_radiation_module.imp_label[12],
            54,
            131.30e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

        #  Tungsten
        impurity_radiation_module.init_imp_element(
            14,
            impurity_radiation_module.imp_label[13],
            74,
            183.85e0,
            frac,
            table_length,
            tmult,
            lzmult,
            errorflag,
        )

    def init_imp_element(
        self, no, label, Z, amass, frac, len_tab, TinkeV, LzinWm3, error
    ):
        """
        Initialises the impurity radiation data for a species
        author: H Lux, CCFE, Culham Science Centre
        author: P J Knight, CCFE, Culham Science Centre
        no      : input integer  : position of species in impurity array
        label   : input string   : species name
        Z       : input integer  : species charge number
        amass   : input real     : species atomic mass (amu)
        frac    : input real     : number density / electron density
        len_tab : input integer  : length of temperature and Lz tables
        TinkeV  : input real     : temperature conversion factor from file to keV
        LzinWm3 : input real     : Lz conversion factor from file to W/m3
        error   : input/output integer : Error flag; 0 = okay, 1 = missing
        impurity data
        This routine initialises the impurity radiation data structure
        for a given impurity species.
        <P>The Lz versus temperature data are read in from file.
        """

        # ###############################################

        if error == 1:
            return

        if no > len(impurity_radiation_module.impurity_arr_Label):
            error_handling.idiags[0] = no
            error_handling.idiags[1] = len(impurity_radiation_module.impurity_arr_Label)
            error_handling.report_error(27)

        impurity_radiation_module.impurity_arr_Label[no] = label
        impurity_radiation_module.impurity_arr_Z[no] = Z
        impurity_radiation_module.impurity_arr_amass[no] = amass
        impurity_radiation_module.impurity_arr_frac[no] = frac
        impurity_radiation_module.impurity_arr_len_tab[no] = len_tab

        if len_tab > (impurity_radiation_module.all_array_hotfix_len):
            print(
                f"ERROR: len_tab is {len_tab} but has a maximum value of {impurity_radiation_module.all_array_hotfix_len}"
            )

        #  Read tabulated data in from file, assuming it exists
        #  Add trailing / to impdir if necessary

        filename = label + "Lzdata.dat"

        if str.index(impurity_radiation_module.impdir(), "/") == len(
            impurity_radiation_module.impdir().strip()
        ):
            fullpath = (impurity_radiation_module.impdir().strip()) + (filename).strip()
        else:
            fullpath = (
                (impurity_radiation_module.impdir().strip()) + "/" + (filename).strip()
            )

        exists(file=(fullpath).strip(), exist=impurity_radiation_module.iexist)

        if impurity_radiation_module.iexist:
            impurity_radiation_module.import_impdata(
                fullpath,
                len_tab,
                impurity_radiation_module.impurity_arr_Temp_keV[no, :],
                impurity_radiation_module.impurity_arr_Lz_Wm3[no, :],
                impurity_radiation_module.impurity_arr_Zav[no, :],
            )
        else:
            raise FileNotFoundError(
                "Warning :  Cannot find impurity data please check path."
            )

        #  Convert tabulated units if necessary

        for i in range(0, len_tab):
            impurity_radiation_module.impurity_arr_Temp_keV[no, i] = (
                impurity_radiation_module.impurity_arr_Temp_keV[no, i] * TinkeV
            )  # keV
            impurity_radiation_module.impurity_arr_Lz_Wm3[no, i] = (
                impurity_radiation_module.impurity_arr_Lz_Wm3[no, i] * LzinWm3
            )  # W/m3
