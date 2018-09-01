#include <fstream>
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
   int ref_time = 0;
   buildTargetTest( ) { 
       // initialization code here
    } 
       //     
    void SetUp() { 
      //  All other targets must be run after cmake --build build 
       struct stat ref_time_stat = checkFileAttributes("bin/process_test.exe"); 
       ref_time = ref_time_stat.st_mtime; // This is reference time. 
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
   struct stat calc_time = checkFileAttributes("documentation/html/build_variables.html"); 
    int curr = calc_time.st_mtime;
    EXPECT_LE(ref_time, curr);
}

TEST_F(buildTargetTest, userguide) { 
   struct stat calc_time = checkFileAttributes("documentation/vardes.pdf"); 
    int curr = calc_time.st_mtime;
    EXPECT_LE(ref_time, curr);
}

