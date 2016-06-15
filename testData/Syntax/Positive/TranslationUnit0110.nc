
// Here we mix a line directive and a function definition.
# 1 "somefile.h"
int print1(const char *format, ...)
{
   char ch = *format;
   return 0;
}
# 2 "somefile.h" 3 4 5 6
int print2(const char *format, ...)
{
   char ch = *format;
   return 0;
}
