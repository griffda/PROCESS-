TEST(Eval, fun_fom) { 
   double a = 1.0;
   double b = 0;
   evaluators_funfom(&a);

//    ASSERT_LT(14.711, val);
//    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
//   ASSERT_DOUBLE_EQ(14.71, val);
    EXPECT_DOUBLE_EQ(b, a);
}

