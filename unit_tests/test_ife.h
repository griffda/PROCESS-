//
//  Tests for IFE model in PROCESS
//  Functions from ife.f90
//
//  Stuart Muldrew
//  UKAEA
//  02/08/19
//
//---------------------------------------

#include <cmath>

TEST(ife, ifetgt_TargFactPow){
    // ifetgt - Routine to calculate the power requirements of the target

    // module and global variables
    extern double reprat, ptargf, tfacmw;

    // Repetition Rate (Hz)
    reprat = 4.0;

    // IFE target factory power at 6 Hz repetition rate
    ptargf = 2.0;

    c_ifetgt();

    EXPECT_NEAR(tfacmw,1.506,0.001);

}