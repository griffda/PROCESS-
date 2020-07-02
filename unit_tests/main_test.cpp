#include <iostream>
#include <gtest/gtest.h>

#include "test_maths_lib.h"
#include "test_costs_1990.h"
#include "test_physics_functions.h"

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
