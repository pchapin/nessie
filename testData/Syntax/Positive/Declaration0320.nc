
// Function definitions are a kind of declaration in the current grammar.

char *f(int x, char *p)
{
  int y = x + 1;
  char buffer[128];
  
  strcpy(buffer, p);
  return p + x;
}
