#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H
//#include <gtest/gtest.h>

#include <fstream>

extern "C"
{
   // Function prototype for accessing Fortran functions 
   double utilities_process_value(double *, double *);
   void evaluators_funfom(double *);
   void availability_calc_u_unplanned_hcd(double *);
   void current_drive_iternb(double *, double *, double *);
   void current_drive_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);
}

struct stat checkFileAttributes(const char* filename)
{
   struct stat result;
   if(stat(filename, &result) == 0)
   {
      auto mod_time = result.st_mtime;
   }
     return result;
}

bool checkFileExists(const char* filename)
{
     std::ifstream Infield(filename);
     return Infield.good();
}
#endif
