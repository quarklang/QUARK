/* Test left-to-right evaluation of expressions */

int a; /* Global variable */

inca() { a = a + 1; return a; }  /* Increment a; return its new value */

main() {
  a = 42;    /* Initialize a */
  print(inca() + a);
}
