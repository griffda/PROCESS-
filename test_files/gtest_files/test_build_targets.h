#include <fstream>
bool checkFileExists(const char* filename);

TEST(Builds, vardes) { 
   bool file_exists = checkFileExists("documentation/html/build_variables.html");
    EXPECT_EQ(1, file_exists);
}

TEST(Builds, userguide) { 
   bool file_exists = checkFileExists("documentation/vardes.pdf");
    EXPECT_EQ(1, file_exists);
}

bool checkFileExists(const char* filename)
{
     std::ifstream Infield(filename);
     return Infield.good();
}
