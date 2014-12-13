#ifndef algor_h__
#define algor_h__

#include "qureg.h"

/*
 *	f(x) = (u * x) mod 2
 * nqubit = nbit + 1 output
 * return found secret_u
 */
uint64_t deutsch_josza_parity(int nbit, uint64_t secret_u, bool dense = true);

/*
 *	Find s such that  f(x) = f(x [+] s) 
 * where [+] is the modulo addition operator
 * return both processed Qureg for inspection and found period
 * non-destructive measurement
 */
std::pair<Qureg, uint64_t> simon_period(int nbit, uint64_t period, bool dense = true);

/*
 *	Find period of a general f(x) = f(x + r)
 */
Qureg qft_period(int nbit, uint64_t period, bool dense = true);

/*
 *	Return both of the factorized prime
 */
std::pair<int, int> shor_factorize(int nbit, int M, bool dense = true);
// Display internal steps
void shor_factorize_verbose(int nbit, int M, bool dense = true);

/*
 *	Return the found key and the sequence of probability at the key
 */
std::pair<uint64_t, vector<float>> grover_search(int nbit, uint64_t key, bool dense = true);

/*
 *	Teleport: demo Bell state entanglement
 * Input a single-qubit register to teleport
 * Return a vector of size 2 [a0, a1]: a0 * |0> + a1 * |1>
 */
vector<CX> teleport(Qureg& singleQureg, bool dense = true);

///////************** Helper functions **************///////
/*
 *	Modular exponentiation
 * b^e mod m
 */
uint64_t exp_mod(uint64_t b, uint64_t e, uint64_t m);

/*
 *	Compute a^p in long long
 */
uint64_t long_pow(uint64_t a, int p);

/*
 *	produce a shor's algorithm oracle
 * f(x) = b^x mod M, where M is the int to be factored
 */
oracle_function shor_oracle(int b, int M);

/*
 *	Find the smallest period such that b^x = 1 mod M
 */
int smallest_period(int b, int M);
#endif // algor_h__