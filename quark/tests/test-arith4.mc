/* Test side-effect sequence in a series of statement */

int g;

main() {
  int l;
  l = 1;
  print(l);
  g = 3;
  print(g);
  l = 5;
  print(l+100);
  g = 7;
  print(g+100);
}