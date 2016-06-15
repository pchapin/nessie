
// A translation unit is a sequence of declarations and line directives. Let's try the basics.

# 1 "somefile.h"
int x;

# 2 "somefile.h"
void f( );

# 3 "somefile.h" 4 5
int print( const char *format, ...);
