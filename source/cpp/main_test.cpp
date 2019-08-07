#include <iostream>
#include <gtest/gtest.h>
extern "C"
{
   double c_process_value_cpp(double *, double *);
}

TEST(Util, process_val) { 
   double a = 12.7;
   double b = 2.01;
   double val = c_process_value_cpp(&a, &b);
   
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
   double val = c_process_value_cpp(&a, &b);
   std::cout<<val<<std::endl;
 return 0;*/
}
