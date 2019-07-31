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

   // Evaluators
   //-----------
   void evaluators_funfom(double *);

   // Availability Module
   //--------------------
   void availability_calc_u_planned(int *, int *, double *);
   void availability_calc_u_unplanned_magnets(int *,int *,double *);
   void availability_calc_u_unplanned_divertor(int *, int *, double *);
   void availability_calc_u_unplanned_fwbs(int *, int *, double *);
   void availability_calc_u_unplanned_hcd(double *);
   void availability_calc_u_unplanned_bop(int *, int *, double *);

   // Costs 1990 Module
   //------------------
   void costs_1990_acc2261();
   void costs_1990_acc2262();
   void costs_1990_acc2263();
   void costs_1990_acc2271();
   void costs_1990_acc2272();
   void costs_1990_acc2273();
   void costs_1990_acc2274();
   void costs_1990_acc228();
   void costs_1990_acc229();
   void costs_1990_acc23();
   void costs_1990_acc241();
   void costs_1990_acc242();
   void costs_1990_acc243();
   void costs_1990_acc244();
   void costs_1990_acc245();
   void costs_1990_acc25();
   void costs_1990_acc26();
   void costs_1990_acc9();

   // Current Drive Module
   //---------------------
   void current_drive_iternb(double *, double *, double *);
   void current_drive_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);

   // Maths Library
   //--------------
   double maths_lib_binomial(int *, int *);

   // Physics functions module
   //-------------------------
   double test_t_eped_scaling();
   double test_plasma_elongation_IPB();
   double test_total_mag_field();
   double test_beta_poloidal();
   double test_res_diff_time();
   
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
