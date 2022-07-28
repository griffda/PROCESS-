import pathlib
import math
import numpy
import pandas
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

        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self.init_imp_element(
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
        self, no, label, z, amass, frac, len_tab, tinkev, lzinwm3, error
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

        if no > len(impurity_radiation_module.impurity_arr_label):
            error_handling.idiags[0] = no
            error_handling.idiags[1] = len(impurity_radiation_module.impurity_arr_label)
            error_handling.report_error(27)

        impurity_radiation_module.impurity_arr_label[no - 1] = label
        impurity_radiation_module.impurity_arr_z[no - 1] = z
        impurity_radiation_module.impurity_arr_amass[no - 1] = amass
        impurity_radiation_module.impurity_arr_frac[no - 1] = frac
        impurity_radiation_module.impurity_arr_len_tab[no - 1] = len_tab

        if len_tab > 100:
            print(
                f"ERROR: len_tab is {len_tab} but has a maximum value of {impurity_radiation_module.all_array_hotfix_len}"
            )

        #  Read tabulated data in from file, assuming it exists
        #  Add trailing / to impdir if necessary

        filename = label.decode("utf-8") + "Lzdata.dat"

        if str.index((impurity_radiation_module.impdir()).decode("utf-8"), "/") == len(
            impurity_radiation_module.impdir().strip()
        ):
            fullpath = (impurity_radiation_module.impdir().strip()) + (filename).strip()
        else:
            fullpath = (
                ((impurity_radiation_module.impdir().strip().decode("utf-8")))
                + "/"
                + (filename).strip()
            )

        my_file = pathlib.Path((fullpath).strip())
        print(my_file)

        if my_file.exists():
            with open(fullpath.strip(), "r") as f:
                impurity_df = pandas.read_csv(
                    f,
                    delimiter="  ",
                    header=1,
                    names=["T", "Lz", "Z_av"],
                    engine="python",
                )

            impurity_radiation_module.impurity_arr_temp_kev[no - 1, :] = impurity_df[
                "T"
            ]
            impurity_radiation_module.impurity_arr_lz_wm3[no - 1, :] = impurity_df["Lz"]
            impurity_radiation_module.impurity_arr_zav[no - 1, :] = impurity_df["Z_av"]
        else:
            raise FileNotFoundError(
                "Warning :  Cannot find impurity data please check path."
            )

        #  Convert tabulated units if necessary

        for i in range(0, len_tab):
            impurity_radiation_module.impurity_arr_temp_kev[no - 1, i] = (
                impurity_radiation_module.impurity_arr_temp_kev[no - 1, i] * tinkev
            )  # keV
            impurity_radiation_module.impurity_arr_lz_wm3[no - 1, i] = (
                impurity_radiation_module.impurity_arr_lz_wm3[no - 1, i] * lzinwm3
            )  # W/m3

    def z2index(self, zimp):

        for i in range(0, len(impurity_radiation_module.impurity_arr_label)):
            if zimp == impurity_radiation_module.impurity_arr_z[i]:
                z2index = i
                return z2index

        # Should only get here if there is a problem

        error_handling.idiags[0] = zimp
        error_handling.report_error(33)

    def element2index(self, element_label: str):

        try:
            return (
                list(impurity_radiation_module.impurity_arr_label).index(
                    element_label.encode("utf-8")
                )
                + 1
            )
        except ValueError:
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
        pbrem = self.pbremden(imp_element_index, ne, te)

        #  Total impurity radiation

        pimp = self.pimpden(imp_element_index, ne, te)

        if pimp >= pbrem:
            pline = pimp - pbrem

        else:  # shouldn't do this... model inconsistency has occurred; okay at high T#
            pline = 0.0e0
            pimp = pbrem
        return pline

    def pbremden(self, imp_element_index, ne, te):

        pbremden = (
            impurity_radiation_module.impurity_arr_frac[imp_element_index - 1]
            * ne
            * ne
            * (self.zav_of_te(imp_element_index, te) ** 2)
            * 5.355e-37
            * numpy.sqrt(te)
        )

        return pbremden

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
                        imp_element_index - 1, i
                    ]
                ) and (
                    te
                    <= impurity_radiation_module.impurity_arr_temp_kev[
                        imp_element_index - 1, i + 1
                    ]
                ):

                    yi = impurity_radiation_module.impurity_arr_zav[
                        imp_element_index - 1, i
                    ]
                    xi = numpy.log(
                        impurity_radiation_module.impurity_arr_temp_kev[
                            imp_element_index - 1, i
                        ]
                    )
                    c = (
                        impurity_radiation_module.impurity_arr_zav[
                            imp_element_index - 1, i + 1
                        ]
                        - yi
                    ) / (
                        numpy.log(
                            impurity_radiation_module.impurity_arr_temp_kev[
                                imp_element_index - 1, i + 1
                            ]
                        )
                        - xi
                    )

                    zav_of_te = yi + c * numpy.log((te) - xi)

        return zav_of_te

    def pimpden(self, imp_element_index, ne, te):

        if (
            te
            <= impurity_radiation_module.impurity_arr_temp_kev[imp_element_index - 1, 0]
        ):

            lz = impurity_radiation_module.impurity_arr_lz_wm3[imp_element_index - 1, 0]

            if (
                not impurity_radiation_module.toolow
            ):  # Only print warning once during a run
                impurity_radiation_module.toolow = True
                error_handling.fdiags[0] = te
                error_handling.report_error(35)

        elif (
            te
            >= impurity_radiation_module.impurity_arr_temp_kev[
                imp_element_index - 1,
                impurity_radiation_module.impurity_arr_len_tab[imp_element_index] - 1,
            ]
        ):
            #  This is okay because Bremsstrahlung will dominate at higher temp.
            lz = impurity_radiation_module.impurity_arr_lz_wm3[
                imp_element_index - 1,
                impurity_radiation_module.impurity_arr_len_tab[imp_element_index] - 1,
            ]

        else:

            for i in range(
                0,
                impurity_radiation_module.impurity_arr_len_tab[imp_element_index] - 1,
            ):

                #  Linear interpolation in log-log space

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
                    print(
                        impurity_radiation_module.impurity_arr_lz_wm3[
                            imp_element_index - 1, i
                        ]
                    )
                    yi = numpy.log(
                        impurity_radiation_module.impurity_arr_lz_wm3[
                            imp_element_index - 1, i
                        ]
                    )
                    xi = numpy.log(
                        impurity_radiation_module.impurity_arr_temp_kev[
                            imp_element_index, i
                        ]
                    )
                    c = (
                        numpy.log(
                            impurity_radiation_module.impurity_arr_lz_wm3[
                                imp_element_index - 1, i + 1
                            ]
                        )
                        - yi
                    ) / (
                        numpy.log(
                            impurity_radiation_module.impurity_arr_temp_kev[
                                imp_element_index, i + 1
                            ]
                        )
                        - xi
                    )
                    lz = math.exp(yi + c * (numpy.log(te) - xi))

                    print(f"numpy.log(te)=  {numpy.log(te)}")

        pimpden = (
            impurity_radiation_module.impurity_arr_frac[imp_element_index - 1]
            * ne
            * ne
            * lz
        )

        return pimpden
