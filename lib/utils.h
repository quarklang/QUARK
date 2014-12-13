#ifndef utils_h__
#define utils_h__

#include <iostream>
#include <string>
#include <complex>
#include <cmath>
#include <vector>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <climits>
#include <algorithm>
#include <memory>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <bitset>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <Eigen/Dense>
#include "frac.h"
#include "prettyprint.h"

using namespace std;
using namespace Eigen;

typedef float REAL;
typedef complex<REAL> CX;
typedef unsigned long long qubase;

#define pr(X) cout << X << endl
#define pause std::cin.get()

// Amplitude tolerance: smaller than this will be considered 0
#define TOL 2e-7
#define PI 3.1415926535897932384626434

// Force inline
#ifdef _MSC_VER
#  define INLINE  __forceinline
#elif defined(__GNUC__)
#  define INLINE  inline __attribute__((always_inline))
#else
#  define INLINE  inline
#endif

/*
*	For initializing static variables only once
*   Variadic macro technique
*/
#define INIT_ONCE(...) \
	static bool init = false; \
	if (!init) { __VA_ARGS__; init = true; }

///////************** Random engine **************///////
INLINE void rand_seed(int seed = -1)
{
	srand(seed < 0 ? time(NULL) : seed);
}

INLINE int rand_int(int low, int high)
{
	return rand() % (high - low) + low;
}

INLINE double rand_double()
{
	return (double)rand() / RAND_MAX;
}

INLINE float rand_float(float low = 0, float high = 1)
{
	float f = (float) rand() / RAND_MAX;
	return low + f * (high - low);
}

INLINE CX rand_cx(float low, float high)
{
	return CX(rand_float(low, high), 
			  rand_float(low, high));
}

// +- symmetric value
INLINE CX rand_cx(float symm = 1)
{
	return CX(rand_float(-symm, symm), 
			  rand_float(-symm, symm));
}

template<int row, int col>
INLINE Matrix<CX, row, col> rand_cxmat(float symm = 1)
{
	Matrix<CX, row, col> m;
	for (int i = 0; i < row ; ++i)
		for (int j = 0; j < col; ++j)
			m(i, j) = rand_cx(symm);
	return m;
}

INLINE MatrixXcf rand_cxmat(int row, int col, float symm = 1)
{
	MatrixXcf m(row, col);
	for (int i = 0; i < row ; ++i)
		for (int j = 0; j < col; ++j)
			m(i, j) = rand_cx(symm);
	return m;
}

/*
 *	Algorithm: choose n ints without replacement from N.
 * http://stackoverflow.com/questions/48087/select-a-random-n-elements-from-listt-in-c-sharp/48089#48089
 */
template<typename IntType>
vector<IntType>& rand_unique(vector<IntType>& out, IntType n, IntType N)
{
	IntType rem = n; // remaining
	IntType outidx = 0;
	IntType ele = 0;
	IntType N_ = N;
	while (rem > 0)
	{
		if (rand_double() <= double(rem) / N_)
		{
			out[outidx ++] = ele;
			--rem;
		}
		++ ele; 
		-- N_;
	}
	return out;
}

template<typename T>
INLINE vector<T>& rand_shuffle(vector<T>& vec)
{
	std::random_shuffle(vec.begin(), vec.end());
	return vec;
}

///////************** Bit operations **************///////
/*
 *	Convert to a bit string
 * May also specify minimal number of bits to print (template parameter)
 */
template<int minbit>
INLINE string bits2str(uint64_t b)
{
	return bitset<minbit>(b).to_string();
}
// Convert to bit string
template<>
INLINE string bits2str<0>(uint64_t b)
{
	string s = bitset<32>(b).to_string();

	// kill leading zeros
	int i = 0;
	while (i < s.size() && s[i] == '0') { ++i; }
	return i != s.size() ? s.substr(i) : "0";
}
INLINE string bits2str(uint64_t b) { return bits2str<0>(b); }
INLINE string bits2str(uint64_t b, int minbit)
{
	string s = bitset<32>(b).to_string();
	return s.substr(32 - minbit);
}

template <typename T, T m, int k>
static INLINE T swapbits(T p)
{
	T q = ((p >> k) ^ p)&m;
	return p^q ^ (q << k);
}
INLINE uint64_t bit_reverse(uint64_t n)
{
	static const uint64_t m0 = 0x5555555555555555ULL;
	static const uint64_t m1 = 0x0300c0303030c303ULL;
	static const uint64_t m2 = 0x00c0300c03f0003fULL;
	static const uint64_t m3 = 0x00000ffc00003fffULL;
	n = ((n >> 1)&m0) | (n&m0) << 1;
	n = swapbits<uint64_t, m1, 4>(n);
	n = swapbits<uint64_t, m2, 8>(n);
	n = swapbits<uint64_t, m3, 20>(n);
	n = (n >> 34) | (n << 30);
	return n;
}

template <bool builtin> // use hardware built-in or not
INLINE int bit_count(size_t b); // Count the bits in a bitmap
// count all the way up to 64. Used less than count to 15
template<>
INLINE int bit_count<false>(size_t b)
{
	b -= (b >> 1) & 0x5555555555555555ULL;
	b = ((b >> 2) & 0x3333333333333333ULL) + (b & 0x3333333333333333ULL);
	b = ((b >> 4) + b) & 0x0F0F0F0F0F0F0F0FULL;
	return (b * 0x0101010101010101ULL) >> 56;
}
// Assembly code that works for both FULL and MAX15
template<>
INLINE int bit_count<true>(uint64_t b)
{
#if defined(_MSC_VER)
	return (int)__popcnt64(b);
#else
	__asm__("popcnt %1, %0" : "=r" (b) : "r" (b));
	return b;
#endif
}

INLINE int bit_count(uint64_t b)
{
	return bit_count<true>(b);
}

/*
 *	Bitwise dot-product
 */
INLINE int bitwise_dot(uint64_t b1, uint64_t b2)
{
	return bit_count(b1 & b2) % 2;
}

/*
 *	Norm of a vector
 */
template<typename T>
INLINE T norm(vector<T> x)
{
	T ans = 0;
	for (T& val : x)
		ans += val * val;
	return sqrt(ans);
}

INLINE CX expi(float theta)
{
	return CX(cos(theta), sin(theta));
}

/*
 *	Hashmap/hashset contains
 */
template<typename Hasher, typename Key>
INLINE bool contains(const Hasher& hasher, Key key)
{
	return hasher.find(key) != hasher.end();
}

/*
 *	max index
 */
template<typename T>
INLINE uint64_t max_index(const vector<T>& vec)
{
	//T mx = -std::numeric_limits<T>::infinity();
	int mi = 0;
	for (int i = 1; i < vec.size() ; ++i)
		if (vec[i] > vec[mi]) mi = i;
	return mi;
}

///////************** For-range loop iterables **************///////
/*
 * inclusive/exclusive: 
 *	Forward: [begin, end)
 * Reverse: (begin, end]
 * In reverse mode, if begin is 0, traverse from the last element
 */
template<typename T, bool forward = true>
struct VecRange
{
	vector<T>& vec;
	size_t beginIdx, endIdx;

	// customized iterator
	struct iter
	{
		size_t i;
		vector<T>& vec;
		iter(vector<T>& _vec, size_t _i) : vec(_vec), i(_i) {}

		T& operator *() { return vec[i]; }

		// prefix
		size_t& operator++() { return forward ? ++i : --i; }

		bool operator==(const iter& other) const
		{
			return this->i == other.i;
		}
		bool operator!=(const iter& other) const
		{
			return this->i != other.i;
		}
	};

	VecRange(vector<T>& _vec, size_t begin, size_t end) :
		vec(_vec), 
		beginIdx(forward ? begin : begin - 1), 
		endIdx(forward ? end : end -1) { }
	VecRange(vector<T>& _vec, size_t begin = 0) :
		vec(_vec),
		beginIdx(forward ? begin : 
				(begin == 0 ? vec.size() : begin) - 1),
				endIdx(forward ? vec.size() : -1) { }

	iter begin() { return iter(vec, beginIdx); }
	iter end() { return iter(vec, endIdx); }
};

/*
 * inclusive/exclusive: 
 *	Forward: [begin, end)
 * Reverse: (begin, end]
 */
template<typename IntType = int, bool forward = true>
struct Range
{
	IntType beginIdx, endIdx;

	// customized iterator
	struct iter
	{
		IntType i;
		iter(IntType _i) : i(_i) {}

		IntType& operator *() { return i; }
		// prefix
		IntType& operator++() { return forward ? ++i : --i; }
		bool operator==(const iter& other) const
		{
			return this->i == other.i;
		}
		bool operator!=(const iter& other) const
		{
			return this->i != other.i;
		}
	};

	Range(IntType begin, IntType end) :
		beginIdx(forward ? begin : begin - 1), 
		endIdx(forward ? end : end -1) { }
	Range(IntType end) :
		beginIdx(forward ? 0 : end - 1), 
		endIdx(forward ? end : -1) { }

	iter begin() { return iter(beginIdx); }
	iter end() { return iter(endIdx); }
};

///////************** Exceptions **************///////
// thrown when requested file can't be opened
class QuantumException : public exception
{
public:
	QuantumException(string fp) : errmsg("Quantum error: ")
	{
		errmsg += fp;
	}
#ifdef _MSC_VER  // VC++ doesn't yet support noexcept()
	virtual const char* what() const throw()
	{
		return errmsg.c_str();
	}
#else  // C++11 noexcept operator. Required by gcc
	virtual const char* what() noexcept(true)
	{
		return errmsg.c_str();
	}
#endif
private:
	string errmsg;
};


///////************** String/printing **************///////

INLINE string int2str(int a)
{
	ostringstream oss;
	oss << a;
	return oss.str();
}

INLINE int str2int(string s)
{
	istringstream iss(s);
	int ans;
	iss >> ans;
	return ans;
}

template<typename T>
string vec2str(vector<T> vec)
{
	ostringstream oss;
	oss << "[";
	for (T& ele : vec)
		oss << ele << ", ";
	string s = oss.str();
	return s.substr(0, s.size() - 2) + "]";
}

template <typename L, typename R>
string concat_space(L left, R right)
{
	ostringstream os;
	os << left << " " << right;
	return os.str();
}
// Must use a start symbol
#define STR string()
INLINE string operator+(string s, int i) { return concat_space(s, i); }
INLINE string operator+(string s, float f) { return concat_space(s, f); }
INLINE string operator+(string s, size_t t) { return concat_space(s, t); }
INLINE string operator+(int i, string s) { return concat_space(i, s); }
INLINE string operator+(float f, string s) { return concat_space(f, s); }
INLINE string operator+(size_t t, string s) { return concat_space(t, s); }

INLINE void ptitle(string title = "") 
{ cout << "！！！！！！！！！！ " << title << " ！！！！！！！！！！！" << endl; }

template<typename T, typename U>
INLINE std::ostream& operator<<(std::ostream& oss, const std::pair<T, U>& p)
{
	return oss << "(" << p.first << ", " << p.second << ")";
}

#endif // utils_h__