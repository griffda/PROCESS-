#include <iostream>
#include <gtest/gtest.h>
extern "C"
{
   double utilities_process_value(double *, double *);
}

TEST(Util, process_val) { 
   double a = 12.7;
   double b = 2.01;
   double val = utilities_process_value(&a, &b);
   
//    ASSERT_LT(14.711, val);
    ASSERT_GT(14.71, val);
//    EXPECT_EQ(14.71, val);
}

int main(int argc, char *argv[])
{
testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();

/*   double a = 12.7;
   double b = 2.01;
   double val = utilities_process_value(&a, &b);
   std::cout<<val<<std::endl;
 return 0;*/
}
