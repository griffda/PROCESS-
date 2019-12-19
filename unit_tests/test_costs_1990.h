//
//  Tests for 1990 Costs model in process
//  Functions from costs.f90
//
//  James Morris
//  UKAEA
//  15/07/19
//
//---------------------------------------

#include <cmath>

TEST(Costs_1990, acc2261_coolant_He_lsa_1) { 
    // acc2261 - Reactor cooling system (He)

    // module and global variables
    extern double fkind, pfwdiv, pnucblkt, pnucshld, pthermmw, c2261;
    extern int coolwh, lsa, nphx;

    // Nth of kind multiplier
    fkind = 1;

    // Safety assurance factor
    lsa = 1;

    // Coolant option - He
    coolwh = 1;

    // 2018 baseline as ref
    // Heat removal from fw and divertor [MW]
    pfwdiv = 0.0;

    // Nuclear power in blanekt and shield [MW]
    pnucblkt = 1558.0;
    pnucshld = 1.478;

    // Total plant thermal power [MW]
    pthermmw = 2647.0;

    // Number of primary heat exchangers
    nphx = 3;

    c_acc2261();

    EXPECT_NEAR(c2261, 49.68, 0.01);
}

TEST(Costs_1990, acc2261_coolant_H2O_lsa_1) { 
    // acc2261 - Reactor cooling system (water)

    // module and global variables
    extern double fkind, pfwdiv, pnucblkt, pnucshld, pthermmw, c2261;
    extern int coolwh, lsa, nphx;

    // Nth of kind multiplier
    fkind = 1;

    // Safety assurance factor
    lsa = 1;

    // Coolant option - H2O
    coolwh = 2;

    // 2018 baseline as ref
    // Heat removal from fw and divertor [MW]
    pfwdiv = 0.0;

    // Nuclear power in blanekt and shield [MW]
    pnucblkt = 1558.0;
    pnucshld = 1.478;

    // Total plant thermal power [MW]
    pthermmw = 2647.0;

    // Number of primary heat exchangers
    nphx = 3;

    c_acc2261();

    EXPECT_NEAR(c2261, 53.85, 0.01);
}

TEST(Costs_1990, acc2262_fkind_1_lsa_4) { 
    // acc2262 - Auxiliary component cooling

    // module and global variables
    extern double fkind, pinjht, crypmw, vachtmw, trithtmw, fachtmw, c2262;
    extern int lsa;

    // Nth of kind multiplier
    fkind = 1;

    // Safety assurance factor
    lsa = 4;

    // Ref is 2018 baseline
    // Power dissipated in HCD system [MW]
    pinjht = 76.5;

    // Cryoplant power [MW]
    crypmw = 39.936;

    // Vacuum pump power [MW]
    vachtmw = 0.5;

    // Tritium processing power requirements [MW]
    trithtmw = 15.0;

    // Facility heat removal [MW]
    fachtmw = 64.835;

    c_acc2262();

    EXPECT_NEAR(c2262, 29.408, 0.01);
}

TEST(Costs_1990, acc2263_fkind_1_lsa_4) { 
    // acc2263 - Cryoplant system

    // module and global variables
    extern double fkind, uccry, tftmp, helpow, c2263;
    extern int lsa;

    // Nth of kind multiplier
    fkind = 1;

    // Safety assurance factor
    lsa = 4;

    // Ref is 2018 baseline
    // Unit cost for heat-transport system cryoplant [$/W**0.67]
    uccry = 9.3e4;

    // Peak He coolant temp in TF and PF [K]
    tftmp = 4.5;

    // Heat removed at cryogenic temperatures [W]
    helpow = 80.980e3;

    c_acc2263();

    EXPECT_NEAR(c2263, 180.76, 0.01);
}

TEST(Costs_1990, acc2271_fkind_1) { 
    // acc2271 - Fuelling system codes

    // module and global variables
    extern double ucf1, fkind, c2271;

    // Nth of kind multiplier
    fkind = 1;

    // cost of fuelling system user input
    ucf1 = 2.23e7;

    c_acc2271();

    EXPECT_NEAR(c2271, 22.3, 0.01);
}

TEST(Costs_1990, acc2272_fkind_1) { 
    // acc2272 - Fuel processing costs

    // module and global variables
    extern double rndfuel, afuel, fkind, c2272;

    // Nth of kind multiplier
    fkind = 1;

    // fuel burn-up rate [reactions/s]
    // baseline 2018 as ref
    rndfuel = 7.158e20;

    // average mass of fuel portion of ions [amu]
    afuel = 2.5;

    c_acc2272();

    EXPECT_NEAR(c2272, 114.707, 0.01);
}

TEST(Costs_1990, acc2273_ttrit_low) { 
    // acc2273 - Atmospheric recovery systems

    // module and global variables
    extern double ftrit, volrci, wsvol, c2273;

    // Fuel tritium fraction
    ftrit = 0.0001;

    c_acc2273();

    EXPECT_NEAR(c2273, 0.0, 0.00001);
}

TEST(Costs_1990, acc2273_ttrit_high) { 
    // acc2273 - Atmospheric recovery systems

    // module and global variables
    extern double ftrit, volrci, wsvol, c2273;

    // Fuel tritium fraction
    ftrit = 0.5;

    // baseline 2018 as ref
    // Internal volume of reactor building [m3]
    volrci = 1299783.4;

    // Volume of warm shop building [m3]
    wsvol = 132304.1;

    c_acc2273();

    EXPECT_NEAR(c2273, 74.12, 0.01);
}

TEST(Costs_1990, acc2274_fkind_1) { 
    // acc2273 - Nuclear building ventilation

    // module and global variables
    extern double volrci, wsvol, fkind, c2274;

    // Nth kind scaling
    fkind = 1.0;

    // baseline 2018 as ref
    // Internal volume of reactor building [m3]
    volrci = 1299783.4;

    // Volume of warm shop building [m3]
    wsvol = 132304.1;

    c_acc2274();

    EXPECT_NEAR(c2274, 84.10, 0.01);
}

TEST(Costs_1990, acc228_fkind_1) { 
    // Instrumentation and control costs

    // module and global variables
    extern double uciac, fkind, c228;

    // Nth of kind multiplier
    fkind = 1;

    c_acc228();

    EXPECT_NEAR(c228, 150.0, 0.01);
}

TEST(Costs_1990, acc228_fkind_0_5) { 
    // Instrumentation and control costs

    // module and global variables
    extern double uciac, fkind, c228;

    // Nth of kind multiplier
    fkind = 0.5;

    c_acc228();

    EXPECT_NEAR(c228, 75.0, 0.01);
}

TEST(Costs_1990, acc229_fkind_1) { 
    // Maintenance equipment costs

    // module and global variables
    extern double ucme, fkind, c229;

    // Nth of kind multiplier
    fkind = 1;

    c_acc229();

    EXPECT_NEAR(c229, 125.0, 0.01);
}

TEST(Costs_1990, acc229_fkind_0_5) { 
    // Maintenance equipment costs

    // module and global variables
    extern double ucme, fkind, c229;

    // Nth of kind multiplier
    fkind = 0.5;

    c_acc229();

    EXPECT_NEAR(c229, 62.5, 0.01);
}

TEST(Costs_1990, acc23_he_coolant) { 
    // Turbine plant equipment costs

    // module and global variables
    extern double pgrossmw, c23;
    extern int coolwh;

    // Coolant choice (1=He,2=H2O)
    coolwh = 1;

    // Gross electric power [MW]
    // Reference value 1200 MW
    pgrossmw = 1200.0;

    c_acc23();

    EXPECT_NEAR(c23, 230, 0.01);
}

TEST(Costs_1990, acc23_h2o_coolant) { 
    // Turbine plant equipment costs

    // module and global variables
    extern double pgrossmw, c23;
    extern int coolwh;

    // Coolant choice (1=He,2=H2O)
    coolwh = 2;

    // Gross electric power [MW]
    // Reference value 1200 MW
    pgrossmw = 1200.0;

    c_acc23();

    EXPECT_NEAR(c23, 245, 0.01);
}

TEST(Costs_1990, acc241_lsa_4) { 
    // Electrical plant - Switchyard

    // module and global variables
    extern double c241;
    extern int lsa;

    // Level of safety assurance
    lsa = 4;

    c_acc241();

    EXPECT_NEAR(c241, 18.4, 0.01);
}

TEST(Costs_1990, acc242_lsa_4) { 
    // Electrical plant - transformers

    // module and global variables
    extern double pacpmw, fcsht, c242;
    int lsa;

    // Safety assurance factor
    lsa = 4;

    // Baseline 2019 as reference
    // Total pulsed power system load [MW]
    pacpmw = 630.0;

    // Total baseline power required at all times [MW]
    fcsht = 65.0;

    c_acc242();

    EXPECT_NEAR(c242, 9.06, 0.01);
}

TEST(Costs_1990, acc243_lsa_4) { 
    // Electrical plant - low voltage

    // module and global variables
    extern double tlvpmw, c243;
    extern int lsa;

    // Safety assurance factor
    lsa = 4;

    // 2018 baseline as ref
    // Estimate of total low-voltage power [MW]
    tlvpmw = 403.8;

    c_acc243();

    EXPECT_NEAR(c243, 8.08, 0.01);
}

TEST(Costs_1990, acc244_lsa_4) { 
    // Account 244: Diesel generator (8 MW per generator, assume 4)

    // module and global variables
    extern double c244;
    extern int lsa;

    // Safety assurance factor
    lsa = 4;

    c_acc244();

    EXPECT_NEAR(c244, 6.80, 0.01);
}

TEST(Costs_1990, acc245_lsa_4) { 
    // Account 245: Auxiliary facility power equipment costs

    // module and global variables
    extern double c245;
    extern int lsa;

    // Safety assurance factor
    lsa = 4;

    c_acc245();

    EXPECT_NEAR(c245, 1.5, 0.01);
}

TEST(Costs_1990, acc25_lsa_4) { 
    // Misc plant costs

    // module and global variables
    extern double ucmisc, c25;
    extern int lsa;

    // Safety assurance factor
    lsa = 4;

    // Miscellaneous plant allowance [$]
    ucmisc = 2.5e7;

    c_acc25();

    EXPECT_NEAR(c25, 25, 0.01);
}

TEST(Costs_1990, acc25_lsa_1) { 
    // Misc plant costs

    // module and global variables
    extern double ucmisc, c25;
    extern int lsa;

    lsa = 1;
    ucmisc = 2.5e7;

    c_acc25();

    EXPECT_NEAR(c25, 19.25, 0.01);
}

TEST(Costs_1990, acc26_ireactor_0_ref) { 
    // Heat rejection system costs

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

    c_acc26();

    // Reference case is for rejected power = 2300 MW
    EXPECT_NEAR(c26, 87.9, 0.01);
}

TEST(Costs_1990, acc26_ireactor_1_ref) { 
    // Heat rejection system costs

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

    c_acc26();

    // Reference case is for rejected power = 2300 MW
    EXPECT_NEAR(c26, 87.9, 0.01);
}

TEST(Costs_1990, acc9_indirt_lsa_4) { 
    // Indirect costs

    // module and global variables
    extern double cindrt, cdirt, cowner;
    extern int lsa;

    // Nth of kind multiplier
    lsa = 4;

    // Plant direct costs [M$]
    cdirt = 30e3;

    // Owner costs
    cowner = 0.15;

    c_acc9();

    EXPECT_NEAR(cindrt, 10005.0, 0.1);
}

TEST(Costs_1990, acc9_ccont_lsa_4) { 
    // Contingency costs

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

    c_acc9();

    EXPECT_NEAR(ccont, 7800.98, 0.1);
}