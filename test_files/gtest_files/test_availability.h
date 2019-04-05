TEST(Availability, calc_u_unplanned_hcd) { 
   double a = 0.0;
   double b = 0.02;
   availability_calc_u_unplanned_hcd(&a);
//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
    EXPECT_NEAR(b, a, 0.0);
}

