/* Bug noticed by Pin-Chin Huang */

fun(x, y)
{
  return 0;
}

main()
{
  int i;
  i = 1;

  fun(i = 2, i = i+1);

  print(i);

}

