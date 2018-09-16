#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H
//#include <gtest/gtest.h>

#include <fstream>
#include <limits>

int min_int = std::numeric_limits<int>::min();

extern "C"
{
   // Function prototype for accessing Fortran functions 
   double utilities_process_value(double *, double *);
   void evaluators_funfom(double *);
   void availability_calc_u_unplanned_hcd(double *);
   void current_drive_iternb(double *, double *, double *);
   void current_drive_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);
}


int elapsed_time(const char* filename)
{
   std::ifstream Infield(filename);
   struct stat result;
   int mod_time = std::numeric_limits<int>::min();
   if(Infield.good())
   {
     if(stat(filename, &result) == 0)
     {
        mod_time = result.st_mtime;
     }
   }
   return mod_time;
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

void run_file_unit_test(int &ref_time, const char* file_name)
{
/* @param ref_time = Last modification time for reference file i.e.
 (file against which tested file is compared for last modification)
  @param file_name = full path of file being compared against reference file
*/
   int min_int = std::numeric_limits<int>::min();
   int calc_time = min_int;
    if(ref_time > min_int)
    {
      calc_time = elapsed_time(file_name); 
      if(ref_time > min_int && calc_time > 0) {
         EXPECT_LE(ref_time, calc_time);
      } else {
        EXPECT_TRUE(false) << "ERROR : File -> "+std::string(file_name)+" not found";
      }
    } else {
      EXPECT_TRUE(false) << "process_test.exe not found! Exiting test";
    }
}


#endif
