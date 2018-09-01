#include <iostream>
#include <gtest/gtest.h>

#include "test_functions.h"
#include "test_util.h"
#include "test_evaluators.h"
#include "test_availability.h"
#include "test_current_drive.h"
#include "test_build_targets.h"
#include "test_build_fixture.h"

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
