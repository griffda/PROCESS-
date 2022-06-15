from genericpath import exists

import numpy
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

    # def import_impdata(self, filename, nlines, col1, col2, col3, skiprows, fmt):
    #     """
    #     Reads two columns of data from file
    #     author: H Lux, CCFE, Culham Science Centre
    #     filename : input char(len=256)     : input filename
    #     nlines   : input integer           : no. of lines to be read
    #     col1(nlines) : output real array   : data in column1
    #     col2(nlines) : output real array   : data in column2
    #     col3(nlines) : output real array   : data in column3
    #     skiprows : optional input integer  : no. of initial rows to skip
    #     fmt      : optional input char(len=256) : data format
    #     This routine reads in the data of a two column file and
    #     returns it. The first two rows are skipped by default.
    #     N/A
    #     """

    #     #  Local variables

    #     unit = 18
    #     i = 0
    #     iostat = 0
    #     in1 = 0
    #     in2 = 0
    #     in3 = 0
    #     # ###############################################

    #     #  Optional variables

    #     if (skiprows) in locals():
    #         local_skip = skiprows
    #     else:
    #         local_skip = 2

    #     if (fmt) in locals() :
    #         local_fmt = fmt
    #     else:
    #         local_fmt = '(3F10.2)'

    #     open(unit=unit, file=(filename).strip(), status='old', action='read', iostat=iostat)

    #     if (iostat != 0) :
    #         error_handling.idiags[0] = iostat
    #         error_handling.report_error(30)

    #     #  Skip first lines (comments)

    #     for i in range(0, local_skip):
    #         read(unit,*,iostat=iostat) buffer
    #     if (iostat > 0) :
    #         error_handling.idiags[0] = iostat
    #         error_handling.report_error(31)

    #     for i in range(0, nlines):
    #         read(unit=unit, fmt=local_fmt, iostat=iostat) in1, in2, in3
    #     if (iostat > 0) :
    #         error_handling.idiags[0] = iostat ;
    #         error_handling.idiags[1] = i
    #         error_handling.report_error(32)
    #     elif  (iostat < 0) :
    #         exit  #  EOF
    #     else:
    #         col1[i-1] = in1
    #         col2[i-1] = in2
    #         col3[i-1] = in3

    #     close(unit)

    def z2index(self, zimp):

        for i in range(0, len(impurity_radiation_module.impurity_arr_label)):
            if zimp == impurity_radiation_module.impurity_arr_Z[i]:
                z2index = i
                return z2index

        # Should only get here if there is a problem

        error_handling.idiags[0] = zimp
        error_handling.report_error(33)

    def element2index(self, element_label):
        i = 0
        for i in range(0, len(impurity_radiation_module.impurity_arr_label)):

            if element_label == impurity_radiation_module.impurity_arr_label[i]:
                element2index = i
                return element2index

            # Should only get here if there is a problem

        error_handling.report_error(34)

    def impradprofile(self, imp_element_index, ne, te, pimp, pbrem, pline):
        """
        Implementation of Bremsstrahlung and loss-function curves
        author: R Kemp, CCFE, Culham Science Centre
        author: H Lux, CCFE, Culham Science Centre
        author: P J Knight, CCFE, Culham Science Centre
        imp_element : input imp_dat : impurity element
        ne    : input real  : electron density (/m3)
        te    : input real  : electron temperature (keV)
        pimp  : output real : total impurity radiation density (W/m3)
        pbrem : output real : Bremsstrahlung radiation density (W/m3)
        pline : output real : other radiation density (W/m3)
        This routine calculates the impurity radiation losses
        for a given temperature and density. Bremsstrahlung equation
        from Johner, L(z) data (coronal equilibrium) from Marco
        Sertoli, Asdex-U, ref. Kallenbach et al.
        Johner, Fusion Science and Technology 59 (2011), pp 308-349
        Sertoli, private communication
        Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041
        """
        pbrem = impurity_radiation_module.pbremden(imp_element_index, ne, te)

        #  Total impurity radiation

        pimp = impurity_radiation_module.pimpden(imp_element_index, ne, te)

        if pimp >= pbrem:
            pline = pimp - pbrem
            return pline
        else:  # shouldn't do this... model inconsistency has occurred; okay at high T#
            pline = 0.0e0
            pimp = pbrem

    def pbremden(self, imp_element_index, ne, te):

        pbremden = (
            impurity_radiation_module.impurity_arr_frac[imp_element_index - 1]
            * ne
            * ne
            * (impurity_radiation_module.zav_of_te(imp_element_index, te) ** 2)
            * 5.355e-37
            * numpy.sqrt(te)
        )

        return pbremden

    # def pimpden(self, imp_element_index, ne, te):

    #     if te <= impurity_radiation_module.impurity_arr_temp_kev[imp_element_index, 1]:
    #         lz = impurity_radiation_module.impurity_arr_lz_wm3[imp_element_index, 1]

    #         if not toolow:
    #             # !  Only print warning once during a run
    #             toolow = True
    #             error_handling.fdiags[0] = te
    #             error_handling.report_error(35)

    #     print(imp_element_index)
    #     print(impurity_radiation_module.impurity_arr_len_tab[imp_element_index])

    #     print(
    #         impurity_radiation_module.impurity_arr_temp_kev[
    #             imp_element_index,
    #             impurity_radiation_module.impurity_arr_len_tab[imp_element_index],
    #         ]
    #     )

    # elif (
    #     te
    #     >= impurity_radiation_module.impurity_arr_temp_kev[
    #         imp_element_index,
    #         impurity_radiation_module.impurity_arr_len_tab[imp_element_index],
    #     ]
    # ):
    #     # !  This is okay because Bremsstrahlung will dominate at higher temp.
    #     lz = impurity_radiation_module.impurity_arr_lz_wm3(
    #         imp_element_index,
    #         impurity_radiation_module.impurity_arr_len_tab(imp_element_index),
    #     )

    # else:

    #     for i in range(
    #         0, impurity_radiation_module.impurity_arr_len_tab(imp_element_index) - 1
    #     ):

    #         # !  Linear interpolation in log-log space

    #         if (
    #             te
    #             > impurity_radiation_module.impurity_arr_temp_kev(
    #                 imp_element_index, i
    #             )
    #         ) and (
    #             te
    #             <= impurity_radiation_module.impurity_arr_temp_kev(
    #                 imp_element_index, i + 1
    #             )
    #         ):

    #             yi = numpy.log(
    #                 impurity_radiation_module.impurity_arr_lz_wm3(
    #                     imp_element_index, i
    #                 )
    #             )
    #             xi = numpy.log(
    #                 impurity_radiation_module.impurity_arr_temp_kev(
    #                     imp_element_index, i
    #                 )
    #             )
    #             c = (
    #                 numpy.log(
    #                     impurity_radiation_module.impurity_arr_lz_wm3(
    #                         imp_element_index, i + 1
    #                     )
    #                 )
    #                 - yi
    #             ) / (
    #                 numpy.log(
    #                     impurity_radiation_module.impurity_arr_temp_kev(
    #                         imp_element_index, i + 1
    #                     )
    #                 )
    #                 - xi
    #             )
    #             lz = numpy.exp(yi + c * (numpy.log(te) - xi))
    #             exit

    # pimpden = (
    #     impurity_radiation_module.impurity_arr_frac(imp_element_index)
    #     * ne
    #     * ne
    #     * lz
    # )
    # return pimpden

    def fradcore(self, rho, coreradius, coreradiationfraction):
        if rho < coreradius:
            fradcore = coreradiationfraction

        else:
            fradcore = 0.0e0

        return fradcore

    def zav_of_te(self, imp_element_index, te):
        if te <= impurity_radiation_module.impurity_arr_temp_kev[imp_element_index, 0]:
            zav_of_te = impurity_radiation_module.impurity_arr_zav[imp_element_index, 0]

        elif (
            te
            >= impurity_radiation_module.impurity_arr_temp_kev[
                imp_element_index,
                (impurity_radiation_module.impurity_arr_len_tab[imp_element_index]) - 1,
            ]
        ):
            zav_of_te = impurity_radiation_module.impurity_arr_zav[
                imp_element_index,
                impurity_radiation_module.impurity_arr_len_tab[imp_element_index] - 1,
            ]
        else:

            for i in range(
                0, impurity_radiation_module.impurity_arr_len_tab[imp_element_index] - 1
            ):

                if (
                    te
                    > impurity_radiation_module.impurity_arr_temp_kev[
                        imp_element_index, i
                    ]
                ) and (
                    te
                    <= impurity_radiation_module.impurity_arr_temp_kev[
                        imp_element_index, i + 1
                    ]
                ):

                    yi = impurity_radiation_module.impurity_arr_zav[
                        imp_element_index, i
                    ]
                    xi = numpy.log(
                        impurity_radiation_module.impurity_arr_temp_kev[
                            imp_element_index, i
                        ]
                    )
                    c = (
                        impurity_radiation_module.impurity_arr_zav[
                            imp_element_index, i + 1
                        ]
                        - yi
                    ) / (
                        numpy.log(
                            impurity_radiation_module.impurity_arr_temp_kev[
                                imp_element_index, i + 1
                            ]
                        )
                        - xi
                    )
                    zav_of_te = yi + c * numpy.log((te) - xi)
                    print(te)
                    print(imp_element_index)

        return zav_of_te
