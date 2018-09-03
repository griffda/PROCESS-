#include <fstream>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef WIN32
#include <unistd.h>
#endif

#ifdef WIN32
#define stat _stat
#endif

class buildTargetTest : public ::testing::Test { 

private:

public: 
   int calc_time = std::numeric_limits<int>::min();;
   int ref_time = calc_time;
   std::string folder = "bin/"; 
   std::string file = "process_GTest.exe";
   buildTargetTest( ) { 
       // initialization code here
    } 
       //     
    void SetUp() { 
      //  All other targets must be run after cmake --build build 
       ref_time = elapsed_time(folder.append(file).c_str()); // This is reference time. 
    }
       //                   
    void TearDown() { 
     // code here will be called just after the test completes
    // ok to through exceptions from here if need be
    }
       //                                        
    ~buildTargetTest( )  { 
    // cleanup any pending stuff, but no exceptions allowed
     }
                                                  
     // put in any custom data members that you need 
};

TEST_F(buildTargetTest, vardes) { 
   folder = "documentation/html/";
   file = "build_variables.html";
   run_file_unit_test(ref_time, folder.append(file).c_str());
}

TEST_F(buildTargetTest, optsolver) { 
   folder = "documentation/pdf/";
   file = "optsolverdoc.pdf";
   run_file_unit_test(ref_time, folder.append(file).c_str());
}

TEST_F(buildTargetTest, userguide) { 
   run_file_unit_test(ref_time, "documentation/pdf/process.pdf");
}

