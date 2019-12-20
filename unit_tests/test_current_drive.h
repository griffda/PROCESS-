TEST(Current_drive, iternb) { 
   double a = 0.01;
   double b = 0.01;
   double c = 0.0;
   c_iternb(&a, &b, &c);
//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
//    ASSERT_NEAR(0.0, a, 0.00);
//    EXPECT_NEAR(0.0, b, 0.00);
    EXPECT_NEAR(0.9867, c, 0.010);
}

TEST(Current_drive, cfnbi) { 
   double afast = 0.01;
   double efast = 0.01;
   double te = 0.01; 
   double ne = 0.01;
   double nd = 0.01;   
   double nt = 0.01;
   double zeffai = 0.01;
   double xlmbda = 0.01;
   double fpion = 0.0;

   c_cfnbi(&afast, &efast, &te, &ne, &nd, &nt, &zeffai, &xlmbda, &fpion);
//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
//    ASSERT_NEAR(0.0, a, 0.00);
    EXPECT_NEAR(0.854, fpion, 0.001);
}

