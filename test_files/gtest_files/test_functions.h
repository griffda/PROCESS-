#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H
#include <gtest/gtest.h>

extern "C"
{
   double utilities_process_value(double *, double *);
   void evaluators_funfom(double *);
   void availability_calc_u_unplanned_hcd(double *);
   void current_drive_iternb(double *, double *, double *);
   void current_drive_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);
}

#endif
