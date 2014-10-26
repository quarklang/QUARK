/* Test all statement forms */

foo(a, b) {
  int i;
  if (a)
    return b + 3;
  else
    for (i = 0 ; i < 5 ; i = i + 1)
       b = b + 5;
  return b;  
}

main() {
  print(foo(1,42));
  print(foo(0,37));
}