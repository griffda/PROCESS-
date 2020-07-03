#include <iostream>
#include <gtest/gtest.h>

#include "test_maths_lib.h"

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
