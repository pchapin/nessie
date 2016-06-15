
struct helper {
  int   x;
  char *p;
};

interface X {
# 29 "./SomeFile.nc"
  command void some_command(struct helper my_helper);
}
