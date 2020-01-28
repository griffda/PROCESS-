//
//  Tests for physics_functions.f90
//
//  James Morris
//  UKAEA
//  15/07/19
//
//---------------------------------------


TEST(Physics_functions, t_eped_scaling_low) { 

    // module and global variables
    extern double triang, kappa, plascur, rmajor, rminor, 
        normalised_total_beta, eped_sf;

    double t_eped;

    // Plasma triangularity
    triang = 0.4;

    // Plasma elongation
    kappa = 1.5;

    // Plasma major radius [m]
    rmajor = 7.0;

    // Plasma minor radius [m]
    rminor = 2.0;

    // Plasma current [A]
    plascur = 10e6;

    // Normalised beta total
    normalised_total_beta = 2.0;

    // eped_sf
    eped_sf = 1.0;

    t_eped = c_t_eped_scaling();

    EXPECT_NEAR(t_eped, 1.7134, 0.001);
}

TEST(Physics_functions, t_eped_scaling_high) { 

    // module and global variables
    extern double triang, kappa, plascur, rmajor, rminor, 
        normalised_total_beta, eped_sf;

    double t_eped;

    // Plasma triangularity
    triang = 0.6;

    // Plasma elongation
    kappa = 2;

    // Plasma major radius [m]
    rmajor = 11.0;

    // Plasma minor radius [m]
    rminor = 3.5;

    // Plasma current [A]
    plascur = 20e6;

    // Normalised beta total
    normalised_total_beta = 3.0;

    // eped_sf
    eped_sf = 1.0;

    t_eped = c_t_eped_scaling();

    EXPECT_NEAR(t_eped, 5.3955, 0.001);
}

TEST(Physics_functions, plasma_elongation_IPB) { 

    // module and global variables
    extern double rminor, rmajor, vol;

    double kappaa_IPB;

    // Plasma major radius [m]
    rmajor = 9.137;

    // Plasma minor radius [m]
    rminor = 2.947;

    // Plasma volume [m3]
    vol = 2634.0;

    kappaa_IPB = c_plasma_elongation_IPB();

    EXPECT_NEAR(kappaa_IPB, 1.682, 0.001);
}

TEST(Physics_functions, total_mag_field) { 

    // module and global variables
    extern double bt, bp;

    double btot;

    // Toroidal magnetic field on-axis [T]
    bt = 5.278;

    // Poloidal magnetic field on-axis [T]
    bp = 0.852;

    btot = c_total_mag_field();

    EXPECT_NEAR(btot, 5.347, 0.001);
}

TEST(Physics_functions, beta_poloidal) { 

    // module and global variables
    extern double btot, beta, bp;

    double betap;

    // Total magnetic field [T]
    btot = 5.347;

    // Plasma beta
    beta = 0.0307;

    // Poloidal magnetic field on-axis [T]
    bp = 0.852;

    betap = c_beta_poloidal();

    EXPECT_NEAR(betap, 1.209, 0.001);
}

TEST(Physics_functions, res_diff_time) { 

    // module and global variables
    extern double rmajor, rplas, kappa95;

    double res_time;

    // Plasma major radius [m]
    rmajor = 9.137;

    // plasma resistance (ohm)
    rplas = 2.909e-9;

    // Plasma elongation at 95% flux surface
    kappa95 = 1.650;

    res_time = c_res_diff_time();

    EXPECT_NEAR(res_time, 4784.3, 0.1);
}