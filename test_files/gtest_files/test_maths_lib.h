//
//  Tests for maths_library.f90
//
//  James Morris
//  Ryan Carreck
//  UKAEA
//  15/07/19
//
//---------------------------------------

TEST(Maths_library, binomial_1) { 
   int n = 1;
   int k = 1;
   double c;
   c = maths_lib_binomial(&n, &k);
   EXPECT_EQ(1.0, c);
}

TEST(Maths_library, binomial_2) { 
   int n = 3;
   int k = 1;
   double c;
   c = maths_lib_binomial(&n, &k);
   EXPECT_EQ(3.0, c);
}

TEST(Maths_library, binomial_3) { 
   int n = 3;
   int k = 3;
   double c;
   c = maths_lib_binomial(&n, &k);
   EXPECT_EQ(1.0, c);
}