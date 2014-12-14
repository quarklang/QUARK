#ifndef qureg_h__
#define qureg_h__

#include "utils.h"

/* convenient for function args */
#define Q Qureg& q
#define Q1 Qureg& q1
#define Q2 Qureg& q2

/* Classical oracle function */
typedef std::function<uint64_t (uint64_t)> oracle_function;

/*
 * Qubits start from most significant bit. 
 */
class Qureg
{
private:
	// Maps a qubase to an index in amp[] array
	unordered_map<qubase, size_t> basemap;
	// non-zero basis, e.g. |00110> and |10100>
	// if basis is empty, we iterate over all 2^nqubit basis
	vector<qubase> basis; 

	/*
	 * Private ctor
	 *	Dense: we don't store the basis explicitly
	 *    Set initBase to amplitude 1. All other amps = 0.
	 * Sparse: we store the basis explicitly
	 *    If init true, we add initBase to amp[] with value 1
	 *    If init false, amp/basis[] will be empty and 'initBase' ignored
	 *    reservedSize for internal allocation
	 */
	Qureg(bool dense, int nqubit, qubase initBase, size_t reservedSize, bool init);

	/*
	 *	Disallow copying. Use clone() explicitly when needed.
	 */
	Qureg(const Qureg&);
	Qureg& operator=(const Qureg&);

public:
	int nqubit; // number of qubits
	bool dense; // if we don't store basis[] explicitly
	vector<CX> amp; // amplitudes

	/*
	 *	Dummy ctor for reference declaration
	 */
	Qureg() {}

	/*
	 *	Move constructor
	 */
	Qureg(Qureg&& other) :
		nqubit(other.nqubit),
		dense(other.dense),
		amp(std::move(other.amp)),
		basemap(std::move(other.basemap)),
		basis(std::move(other.basis)) { }

	Qureg& operator=(Qureg&& other)
	{
		nqubit = other.nqubit;
		dense = other.dense;
		amp = std::move(other.amp);
		basemap = std::move(other.basemap);
		basis = std::move(other.basis);
		return *this;
	}

	/**********************************************
	* Creation ctors  *
	* will be defined outside the class {}
	**********************************************/
	template<bool dense>
	static Qureg create(int nqubit, unsigned long long = 0);
	
	template<bool dense>
	static Qureg create(int nqubit, size_t reservedSize, qubase initBase);

	/**********************************************/
	/*********** Dense ONLY  ***********/
	/**********************************************/
	INLINE void set_base_d(qubase base, CX a)
	{
		amp[base] = a;
	}

	/*
	 * Iterate over all basis from 0 to 1<<nqubit
	 */
#define DENSE_ITER(base) for (qubase base = 0; base < (1<<q.nqubit); ++base)

	/**********************************************/
	/*********** Sparse ONLY  ***********/
	/**********************************************/
	/*
	 *	Test if a base already exists in basis[]
	 */
	INLINE bool contains_base(qubase base)
	{
		return contains(basemap, base);
	}

	/*
	 *	Add a base. Processes hashmap. Sparse ONLY. 
	 */
	INLINE void add_base(qubase base, CX a)
	{
		basemap[base] = amp.size();
		basis.push_back(base);
		amp.push_back(a);
	}

	/*
	 * Read index from basemap and get amplitude
	 */
	INLINE CX& operator[](qubase base) { return amp[basemap[base]]; }

	/*
	 *	For-each loop over basis[]. You can append to basis[] as you iterate
	 */
	INLINE VecRange<qubase> base_iter_s() { return VecRange<qubase>(basis); }

	/*
	 *	Sort the basis vectors. 
	 * The amp array is NOT sorted, cause basemap takes care of indices
	 */
	Qureg& sort()
	{
		if (!dense)
			std::sort(basis.begin(), basis.end());
		return *this;
	}

	/*
	 *	Remove near-zero amplitude, tolerance defined by TOL
	 */
	Qureg& purge();

	/**********************************************/
	/*********** Common part  ***********/
	/**********************************************/
	/*
	*	Size of complex amplitude vector
	*/
	INLINE size_t size() { return amp.size(); }

	/*
	 *	Explicit copying
	 */
	Qureg clone();

	/*
	 *	Get base stored at an internal index
	 * dense and sparse
	 */
	INLINE qubase get_base_internal(size_t i) { return dense ? i : basis[i]; }

	/*
	 *	Get a bit string representing a target qubit
	 * Most significant bit
	 */
	INLINE qubase to_qubase(int tar)
	{
		return qubase(1) << (nqubit - 1 - tar);
	}

	/*
	 *	If nonZeroOnly true, prints only states with non-zero amp
	 * default true
	 */
	string to_string(bool nonZeroOnly = true);

	operator string()
	{
		return to_string(true);
	}

	friend ostream& operator<<(ostream& os, Q)
	{
		return os << string(q);
	}

	/*
	*	Add scratch bits to 'this'. (Add to least significant bits)
	*/
	Qureg& operator+=(int scratchQubits);

	/*
	 *	Convert to a column vector of 2^nqubit size
	 */
	operator VectorXcf();

	/*
	 *	Vector of states with non-zero amplitude
	 */
	vector<qubase> non_zero_states();

	INLINE CX get_amp(const qubase& base)
	{
		return dense ? amp[base] : 
			contains_base(base) ? (*this)[base] : CX(0);
	}

	/*
	 *	Pairs of non-zero states and their corresponding probability, 
	 * sorted in descending order
	 */
	vector<pair<qubase, float>> sorted_non_zero_states();

	/*
	 *	Get the sum of probability of all bases with 'prefix'
	 * nbit: prefix length
	 */
	float prefix_prob(int nbit, qubase prefix);

	///////************** Quantum operations **************///////
	/*
	 *	Measure the whole register
	 */
	friend qubase measure(Q);
	/*
	 *	Return either 0 or 1
	 */
	friend int measure(Q, int tar, bool destructive = true);
	/*
	 *	top n qubits
	 * destructive, if true (which is physically the case) will alter the qureg state
	 */
	friend uint64_t measure_top(Q, int topSize, bool destructive = true);
	/*
	 *	from start to start+size-1 qubits
	 * destructive, if true (which is physically the case) will alter the qureg state
	 */
	friend uint64_t measure_range(Q, int startBit, int qsize, bool destructive = true);

	/*
	 *	Apply an int -> int classical oracle on this register
	 * inputQubits: how many most significant bits to be taken as input
	 * take |x>|b> and map to |x>|b xor f(x)>
	 */
	friend void apply_oracle(Q, const oracle_function& oracle, int inputQubits);
};


// Move the specializations outside because GCC doesn't allow it
/*
*	Create a dense Qureg, amp[initBase] = 1 while all others 0
*/
template<>
inline Qureg Qureg::create<true>(int nqubit, qubase initBase)
{
	return Qureg(true, nqubit, initBase, 0, false);
}
/*
*	Create a sparse Qureg, all amps = 0.
*/
template<>
inline Qureg Qureg::create<false>(int nqubit, unsigned long long reservedSize)
{
	return Qureg(false, nqubit, 0, reservedSize, false);
}

/*
*	Create a dense Qureg, amp[initBase] = 1 while all others 0
*/
template<>
inline Qureg Qureg::create<false>(int nqubit, size_t reservedSize, qubase initBase)
{
	return Qureg(false, nqubit, initBase, reservedSize, true);
}

#endif // qureg_h__
