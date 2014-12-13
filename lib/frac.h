#ifndef frac_h__
#define frac_h__

#include <iostream>
#include <string>
#include <cmath>
#include <cstdlib>
#include <memory>
#include <cmath>
using namespace std;

inline uint64_t gcd(uint64_t a, uint64_t b)
{
	uint64_t c;
	while (a != 0)
	{
		c = a;
		a = b % a;
		b = c;
	}
	return b;
}

class Frac
{
public:
	int64_t num, denom;

	Frac()
	{
		num = 0;
		denom = 1;
	}

	Frac(int64_t n)
	{
		num = n;
		denom = 1;
	}

	Frac(int64_t n, int64_t d)
	{
		if (d == 0)
		{
			cerr << "Denominator may not be 0." << endl;
			exit(0);
		}
		else if (n == 0)
		{
			num = 0;
			denom = 1;
		}
		else
		{
			int sign = 1;
			if (n < 0)
			{
				sign *= -1;
				n *= -1;
			}
			if (d < 0)
			{
				sign *= -1;
				d *= -1;
			}

			long long tmp = gcd(n, d);
			num = n / tmp*sign;
			denom = d / tmp;
		}
	}

	Frac& operator=(const Frac& other)
	{
		this->num = other.num;
		this->denom = other.denom;
		return *this;
	}

	operator int() { return int(num / denom); }
	operator int64_t() { return num / denom; }
	operator float() { return ((float)num) / denom; }
	operator double() { return ((double)num) / denom; }
};

inline bool operator==(const Frac& lhs, const Frac& rhs)
{
	return lhs.num == rhs.num && lhs.denom == rhs.denom;
}

inline bool operator!=(const Frac& lhs, const Frac& rhs)
{
	return !(lhs == rhs);
}

#define GEN_RELATIONAL(op) \
inline bool operator op (const Frac& lhs, const Frac& rhs) \
{ \
	return lhs.num*rhs.denom - rhs.num*lhs.denom op 0; \
}

GEN_RELATIONAL(>)
GEN_RELATIONAL(>=)
GEN_RELATIONAL(<)
GEN_RELATIONAL(<=)

inline Frac operator+(const Frac& lhs, const Frac& rhs)
{
	return Frac(lhs.num*rhs.denom
				 + rhs.num*lhs.denom,
				 lhs.denom*rhs.denom);
}

inline Frac operator+(const Frac& lhs, const int64_t& rhs)
{
	return Frac(lhs.num
				+ rhs*lhs.denom,
				lhs.denom);
}

inline Frac operator+(const int64_t& lhs, const Frac& rhs)
{
	return Frac(lhs*rhs.denom
				+ rhs.num,
				rhs.denom);
}

inline Frac operator+=(Frac& lhs, const Frac& rhs)
{
	Frac tmp(lhs.num*rhs.denom
				 + rhs.num*lhs.denom,
				 lhs.denom*rhs.denom);
	lhs = tmp;
	return lhs;
}

inline Frac operator-(const Frac& lhs, const Frac& rhs)
{
	return Frac(lhs.num*rhs.denom
				 - rhs.num*lhs.denom,
				 lhs.denom*rhs.denom);
}

inline Frac operator-(const Frac& lhs, const int64_t& rhs)
{
	return Frac(lhs.num
				- rhs*lhs.denom,
				lhs.denom);
}

inline Frac operator-(const int64_t& lhs, const Frac& rhs)
{
	return Frac(lhs*rhs.denom
				- rhs.num,
				rhs.denom);
}

inline Frac operator-=(Frac& lhs, const Frac& rhs)
{
	Frac tmp(lhs.num*rhs.denom
				 - rhs.num*lhs.denom,
				 lhs.denom*rhs.denom);
	lhs = tmp;
	return lhs;
}

inline Frac operator*(const Frac& lhs, const Frac& rhs)
{
	return Frac(lhs.num*rhs.num,
				 lhs.denom*rhs.denom);
}

inline Frac operator*(const int64_t& lhs, const Frac& rhs)
{
	return Frac(lhs*rhs.num, rhs.denom);
}

inline Frac operator*(const Frac& rhs, const int64_t& lhs)
{
	return Frac(lhs*rhs.num, rhs.denom);
}

inline Frac operator*=(Frac& lhs, const Frac& rhs)
{
	Frac tmp(lhs.num*rhs.num,
				 lhs.denom*rhs.denom);
	lhs = tmp;
	return lhs;
}

inline Frac operator/(const Frac& lhs, const Frac& rhs)
{
	return Frac(lhs.num*rhs.denom,
				 lhs.denom*rhs.num);
}

inline Frac operator/(const Frac& lhs, const int64_t& rhs)
{
	return Frac(lhs.num,
				lhs.denom*rhs);
}

inline Frac operator/(const int64_t& lhs, const Frac& rhs)
{
	return Frac(lhs*rhs.denom,
				rhs.num);
}

// reciprocal
inline Frac operator~(const Frac& f)
{
	return Frac(f.denom, f.num);
}

inline std::ostream& operator<<(std::ostream &strm, const Frac &f)
{
	if (f.denom == 1)
		strm << f.num;
	else
		strm << f.num << "/" << f.denom;
	return strm;
}

///////************** Continued fraction **************///////
typedef vector<int64_t> ContFrac;

inline Frac to_frac(const ContFrac& cfrac, int size = 0)
{
	if (size < 1)
		size = cfrac.size();
	Frac ans(1, cfrac[size - 1]);
	for (int i = size - 2; i >= 1; --i)
		ans = ~(ans + cfrac[i]);
	return ans + cfrac[0];
}

/*
 *	If size == 0, continue until 0
 */
inline ContFrac to_cont_frac(Frac frac, int size = 0)
{
	ContFrac cfrac;
	if (size > 0)
		cfrac.reserve(size);
	int i = 0;
	while (size < 1 || i < size)
	{
		cfrac.push_back(int64_t(frac));
		frac -= cfrac[i];
		if (frac.num == 0) break;
		frac = ~frac;
		++ i;
	}
	return cfrac;
}

#endif // frac_h__
