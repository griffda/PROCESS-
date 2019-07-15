//
//  Tests for 1990 Costs model in process
//  Functions from costs.f90
//
//  James Morris
//  UKAEA
//  15/07/19
//
//---------------------------------------

TEST(Costs_1990, acc9_indirt_lsa_4) { 

    // module and global variables
    extern double cindrt, cdirt, cowner;
    extern int lsa;

    // Nth of kind multiplier
    lsa = 4;

    // Plant direct costs [M$]
    cdirt = 30e3;

    // Owner costs
    cowner = 0.15;

    costs_1990_acc9();

    EXPECT_NEAR(cindrt, 10005.0, 0.1);
}

TEST(Costs_1990, acc9_ccont_lsa_4) { 

    // module and global variables
    extern double cindrt, cdirt, cowner, fcontng, ccont;
    extern int lsa;

    // Nth of kind multiplier
    lsa = 4;

    // Owner costs
    cowner = 0.15;

    // Plant direct costs [M$]
    cdirt = 30e3;

    // Contingency factor
    fcontng = 0.195;

    costs_1990_acc9();

    EXPECT_NEAR(ccont, 7800.98, 0.1);
}

TEST(Costs_1990, acc228_fkind_1) { 

    // module and global variables
    extern double uciac, fkind, c228;

    // Nth of kind multiplier
    fkind = 1;

    costs_1990_acc228();

    EXPECT_NEAR(c228, 150.0, 0.01);
}

TEST(Costs_1990, acc228_fkind_0_5) { 

    // module and global variables
    extern double uciac, fkind, c228;

    // Nth of kind multiplier
    fkind = 0.5;

    costs_1990_acc228();

    EXPECT_NEAR(c228, 75.0, 0.01);
}

TEST(Costs_1990, acc229_fkind_1) { 

    // module and global variables
    extern double ucme, fkind, c229;

    // Nth of kind multiplier
    fkind = 1;

    costs_1990_acc229();

    EXPECT_NEAR(c229, 125.0, 0.01);
}

TEST(Costs_1990, acc229_fkind_0_5) { 

    // module and global variables
    extern double ucme, fkind, c229;

    // Nth of kind multiplier
    fkind = 0.5;

    costs_1990_acc229();

    EXPECT_NEAR(c229, 62.5, 0.01);
}

TEST(Costs_1990, acc23_he_coolant) { 

    // module and global variables
    extern double pgrossmw, c23;
    extern int coolwh;

    // Coolant choice (1=He,2=H2O)
    coolwh = 1;

    // Gross electric power [MW]
    // Reference value 1200 MW
    pgrossmw = 1200.0;

    costs_1990_acc23();

    EXPECT_NEAR(c23, 230, 0.01);
}

TEST(Costs_1990, acc23_h2o_coolant) { 

    // module and global variables
    extern double pgrossmw, c23;
    extern int coolwh;

    // Coolant choice (1=He,2=H2O)
    coolwh = 2;

    // Gross electric power [MW]
    // Reference value 1200 MW
    pgrossmw = 1200.0;

    costs_1990_acc23();

    EXPECT_NEAR(c23, 245, 0.01);
}

TEST(Costs_1990, acc25_lsa_4) { 

    // module and global variables
    extern double ucmisc, c25;
    extern int lsa;

    // Safety assurance factor
    lsa = 4;

    // Miscellaneous plant allowance [$]
    ucmisc = 2.5e7;

    costs_1990_acc25();

    EXPECT_NEAR(c25, 25, 0.01);
}

TEST(Costs_1990, acc25_lsa_1) { 

    // module and global variables
    extern double ucmisc, c25;
    extern int lsa;

    lsa = 1;
    ucmisc = 2.5e7;

    costs_1990_acc25();

    EXPECT_NEAR(c25, 19.25, 0.01);
}

TEST(Costs_1990, acc26_ireactor_0_ref) { 

    // module and global variables
    extern double powfmw, pinjwp, tfcmw, pthermmw, pgrossmw, c26;
    extern int lsa, ireactor;

    // Safety assurance factor
    lsa = 4;

    // Reactor boolean
    ireactor = 0;

    // Fusion power [MW]
    powfmw = 2000.0;

    // Wall-plug power for injected power [MW]
    pinjwp = 250.0;

    // Peak power per TF power supply [MW]
    tfcmw = 50.0;

    costs_1990_acc26();

    // Reference case is for rejected power = 2300 MW
    EXPECT_NEAR(c26, 87.9, 0.01);
}

TEST(Costs_1990, acc26_ireactor_1_ref) { 

    // module and global variables
    extern double powfmw, pinjwp, tfcmw, pthermmw, pgrossmw, c26;
    extern int lsa, ireactor;

    // Safety assurance factor
    lsa = 4;

    // Reactor boolean
    ireactor = 1;

    // Thermal power generated [MW]
    pthermmw = 3000.0;

    // Gross electrical power generated [MW]
    pgrossmw = 700.0;

    costs_1990_acc26();

    // Reference case is for rejected power = 2300 MW
    EXPECT_NEAR(c26, 87.9, 0.01);
}