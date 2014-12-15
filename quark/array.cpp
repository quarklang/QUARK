#include "qureg.h"
#include "qumat.h"
#include "qugate.h"
#include "quarklang.h"

using namespace Qumat;
using namespace Qugate;

void array_test()
{
vector<int64_t> arr1 = vector<int64_t>{ 0, 1, 2, 3, 4, 5 }; 
int64_t x; 
int64_t _QUARK_5H0aq5mw6x = len(arr1);
int64_t _QUARK_v3YH0O1B0h = 1;
int64_t _QUARK_l03AMaXh6u = _QUARK_v3YH0O1B0h > 0 ? 1 : -1;
for (x = 0; _QUARK_l03AMaXh6u * x < _QUARK_l03AMaXh6u * len(arr1); x += _QUARK_v3YH0O1B0h){
std::cout << std::boolalpha << std::setprecision(6) << x << std::endl;
} // end for-range
} // end array_test()
