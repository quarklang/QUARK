int a;
int b;

printa()
{
  print(a);
}

printb()
{
  print(b);
}

incab()
{
  a = a + 1;
  b = b + 1;
}

main()
{
  a = 42;
  b = 21;
  printa();
  printb();
  incab();
  printa();
  printb();
}
