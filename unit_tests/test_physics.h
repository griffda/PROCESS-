//
//  Tests for physics.f90
//
//  Stuart Muldrew
//  UKAEA
//  10/02/20
//
//---------------------------------------


TEST(Physics, diamagnetic_fraction_hender_1) { 

    // module and global variables

    // Beta
    double a=0.14;

    // Diamagnetic fraction
    double result;

    c_diamagnetic_fraction_hender(&a, &result);

    EXPECT_NEAR(result, 0.0500, 0.0001);
}

TEST(Physics, diamagnetic_fraction_scene_1) { 

    // module and global variables

    // Beta
    double a=0.15;

    // q95
    double b=3.0;

    // q0
    double c=1.0; 

    // Diamagnetic fraction
    double result;

    c_diamagnetic_fraction_scene(&a, &b, &c, &result);

    EXPECT_NEAR(result, 0.0460, 0.0001);
}

TEST(Physics, ps_fraction_scene_1) { 

    // module and global variables

    // Beta
    double a=0.15;

    // Diamagnetic fraction
    double result;


    c_ps_fraction_scene(&a, &result);

    EXPECT_NEAR(result, -0.0135, 0.0001);
}

