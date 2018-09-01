#include <sys/types.h>
#include <sys/stat.h>
#ifndef WIN32
#include <unistd.h>
#endif

#ifdef WIN32
#define stat _stat
#endif


TEST(Builds, vardes) { 
//   bool file_exists = checkFileExists("documentation/html/build_variables.html");
   bool file_exists = checkFileExists("bin/process_test.exe");
    EXPECT_EQ(1, file_exists);
}

TEST(Builds, userguide) { 
   bool file_exists = checkFileExists("documentation/pdf/process.pdf");
    EXPECT_EQ(1, file_exists);
}

