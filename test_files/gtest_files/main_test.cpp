#include <iostream>
#include <gtest/gtest.h>

#include "test_functions.h"
#include "test_util.h"
#include "test_evaluators.h"
#include "test_availability.h"
#include "test_current_drive.h"

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
