
  int scheck(int r) __attribute__((noinline)) {
    if (r != SUCCESS)
      fail(r);
    return r == SUCCESS;
  }
