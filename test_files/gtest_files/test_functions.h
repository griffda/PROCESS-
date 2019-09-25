#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H
//#include <gtest/gtest.h>

#include <fstream>
#include <limits>

int min_int = std::numeric_limits<int>::min();

extern "C"
{
   // Function prototype for accessing Fortran functions 
   double c_process_value_cpp(double *, double *);

   // Evaluators
   //-----------
   void c_funfom(double *);

   // Availability Module
   //--------------------
   void c_calc_u_planned(int *, int *, double *);
   void c_calc_u_unplanned_magnets(int *,int *,double *);
   void c_calc_u_unplanned_divertor(int *, int *, double *);
   void c_calc_u_unplanned_fwbs(int *, int *, double *);
   void c_calc_u_unplanned_hcd(double *);
   void c_calc_u_unplanned_bop(int *, int *, double *);

   // Costs 1990 Module
   //------------------
   void c_acc2261();
   void c_acc2262();
   void c_acc2263();
   void c_acc2271();
   void c_acc2272();
   void c_acc2273();
   void c_acc2274();
   void c_acc228();
   void c_acc229();
   void c_acc23();
   void c_acc241();
   void c_acc242();
   void c_acc243();
   void c_acc244();
   void c_acc245();
   void c_acc25();
   void c_acc26();
   void c_acc9();

   // Current Drive Module
   //---------------------
   void c_iternb(double *, double *, double *);
   void c_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);

   // Maths Library
   //--------------
   double c_binomial(int *, int *);

   // Physics functions module
   //-------------------------
   double c_t_eped_scaling();
   double c_plasma_elongation_IPB();
   double c_total_mag_field();
   double c_beta_poloidal();
   double c_res_diff_time();
  
   // IFE module
   //-------------------------
   void c_ifetgt();

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
