/* Test right-to-left evaluation of arguments */

int a; /* Global variable */

inca() { a = a + 1; return a; }  /* Increment a; return its new value */

add2(x, y) { return x + y; }

main() {
  a = 0;
  print(add2(inca(), a));
}
