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

    t_eped = test_t_eped_scaling();

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

    t_eped = test_t_eped_scaling();

    EXPECT_NEAR(t_eped, 5.3955, 0.001);
}