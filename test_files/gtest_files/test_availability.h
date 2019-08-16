//
//  Tests for physics_functions.f90
//
//  James Morris
//  Ryan Carreck
//  UKAEA
//  15/07/19
//
//---------------------------------------

TEST(Availability, calc_u_unplanned_hcd) { 
   double a = 0.0;
   double b = 0.02;
   c_calc_u_unplanned_hcd(&a);
//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
    EXPECT_NEAR(b, a, 0.0);
}

TEST(Availability, calc_u_unplanned_bop) { 
   int a = 0;
   int b = 0;
   double result;

   // module and global variables
   extern double t_operation;
   t_operation = 25.0;

   c_calc_u_unplanned_bop(&a, &b, &result);
   EXPECT_NEAR(result, 0.009, 0.0005);
}

TEST(Availability, calc_u_planned_nominal){
   int a = 0;
   int b = 0;
   double result;

   extern double  bktlife,
                  divlife,
                  abktflnc,
                  wallmw,
                  tlife,
                  cdrlife,
                  adivflnc,
                  hldiv,
                  cplife,
                  cpstflnc;
   extern int     num_rh_systems;
      //estimates from 2019 baseline to attain an order of magnitude
      abktflnc = 5.0;
      adivflnc = 10.0;
      tlife = 30.0;
      num_rh_systems = 5;
      wallmw = 1.0;
      hldiv = 10.0;

   c_calc_u_planned(&a, &b, &result);
   EXPECT_NEAR(result, 0.3 ,0.05);
}

TEST(Availability, calc_u_planned_nominal_ST){
   int a = 0;
   int b = 0;
   double result;

   // module and global variables
   extern double  abktflnc,
                  wallmw,
                  tlife,
                  adivflnc,
                  hldiv,
                  cpstflnc;
   extern int     num_rh_systems,
                  itart;
      //estimates from FNSF baseline to attain an order of magnitude
      abktflnc = 20.0;
      adivflnc = 25.0;
      cpstflnc = 20.0;
      tlife = 30.0;
      num_rh_systems = 4;
      wallmw = 1.0;
      hldiv = 1.0;
      itart = 1;


   c_calc_u_planned(&a, &b, &result);
   EXPECT_NEAR(result, 0.03 ,0.005);
}

TEST(Availability, calc_u_unplanned_magnets_no_degredation){
   int a = 0;
   int b = 0;
   double result;

   extern double  temp_margin,
                  tmargmin_tf,
                  tmargmin_cs,
                  t_operation,
                  conf_mag;

   temp_margin = 1.5;
   tmargmin_tf = 1.5;
   tmargmin_cs = 1.5;
   t_operation = 30;
   conf_mag = 1.0;         


   c_calc_u_unplanned_magnets(&a, &b, &result);
   EXPECT_NEAR(result, 0.02 ,0.005);
}

TEST(Availability, calc_u_unplanned_magnets_no_degredation_conf){
   int a = 0;
   int b = 0;
   double result;

   extern double  temp_margin,
                  tmargmin_tf,
                  tmargmin_cs,
                  t_operation,
                  conf_mag;

   temp_margin = 2.0;
   tmargmin_tf = 1.6;
   tmargmin_cs = 1.6;
   t_operation = 30;
   conf_mag = 0.8;         


   c_calc_u_unplanned_magnets(&a, &b, &result);
   EXPECT_NEAR(result, 0.02 ,0.005);
}

TEST(Availability, calc_u_unplanned_magnets_degredation_conf){
   int a = 0;
   int b = 0;
   double result;

   extern double  temp_margin,
                  tmargmin_tf,
                  tmargmin_cs,
                  t_operation,
                  conf_mag;

   temp_margin = 1.8;
   tmargmin_tf = 1.6;
   tmargmin_cs = 1.6;
   t_operation = 30;
   conf_mag = 0.8;         


   c_calc_u_unplanned_magnets(&a, &b, &result);
   EXPECT_NEAR(result, 0.03 ,0.005);
}

TEST(Availability, calc_u_unplanned_divertor_below_nref){
   int a = 0;
   int b = 0;
   double result;

   extern double  divlife,
                  tcycle;

   divlife = 1.99;
   tcycle = 9000;


   c_calc_u_unplanned_divertor(&a, &b, &result);
   EXPECT_NEAR(result, 0.02 ,0.005);
}

TEST(Availability, calc_u_unplanned_divertor_above_nu){
   int a = 0;
   int b = 0;
   double result;

   extern double  divlife,
                  tcycle;

   divlife = 4;
   tcycle = 9000;


   c_calc_u_unplanned_divertor(&a, &b, &result);
   EXPECT_NEAR(result, 1 ,0);
}

TEST(Availability, calc_u_unplanned_divertor_between){
   int a = 0;
   int b = 0;
   double result;

   extern double  divlife,
                  tcycle;

   divlife = 3;
   tcycle = 9000;


   c_calc_u_unplanned_divertor(&a, &b, &result);
   EXPECT_NEAR(result, 0.1 ,0.05);
}

TEST(Availability, calc_u_unplanned_fwbs_below_nref){
   int a = 0;
   int b = 0;
   double result;

   extern double  bktlife,
                  tcycle;

   bktlife = 5;
   tcycle = 9000;


   c_calc_u_unplanned_fwbs(&a, &b, &result);
   EXPECT_NEAR(result, 0.02 ,0.005);
}

TEST(Availability, calc_u_unplanned_fwbs_above_nu){
   int a = 0;
   int b = 0;
   double result;

   extern double  bktlife,
                  tcycle;

   bktlife = 15;
   tcycle = 9000;


   c_calc_u_unplanned_fwbs(&a, &b, &result);
   EXPECT_NEAR(result, 1 ,0);
}

TEST(Availability, calc_u_unplanned_fwbs_between){
   int a = 0;
   int b = 0;
   double result;

   extern double  bktlife,
                  tcycle;

   bktlife = 8.5;
   tcycle = 9000;


   c_calc_u_unplanned_fwbs(&a, &b, &result);
   EXPECT_NEAR(result, 0.1 ,0.005);
}