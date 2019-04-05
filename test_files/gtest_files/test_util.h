TEST(Util, process_val) { 
   double a = 12.7;
   double b = 2.01;
   double val = utilities_process_value(&a, &b);

//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
    EXPECT_NEAR(14.711, val, 0.01);
}

