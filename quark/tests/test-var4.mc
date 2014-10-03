int a;

foo(b) {
  int c;
  c = a;
  print(c);
  a = b;
  print(a);
}

main() {
  a = 12;
  foo(42);
}
