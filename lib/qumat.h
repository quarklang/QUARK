#ifndef qumat_h__
#define qumat_h__

#include "qureg.h"
namespace Qumat // quantum matrices
{
	/*
	 *	Kronecker product
	 * resultDense: true to return a dense Qureg
	 */
	Qureg kronecker(Q1, Q2, bool resultDense);

	/*
	 *	Kronecker product
	 * Result Qureg is dense only when both q1 and q2 are dense.
	 */
	INLINE Qureg operator&(Q1, Q2)
	{
		return kronecker(q1, q2, q1.dense && q2.dense);
	}

	/**********************************************/
	/*********** Eigen operations  ***********/
	/**********************************************/
	Matrix2cf hadamard_mat();
	MatrixXcf hadamard_mat(int nqubit);

	MatrixXcf kronecker_mat(const MatrixXcf& A, const MatrixXcf& B);

	INLINE MatrixXcf 
		operator&(const MatrixXcf& A, const MatrixXcf& B)
	{
		return kronecker_mat(A, B);
	}

	Matrix4cf cnot_mat();

	Matrix<CX, 8, 8> toffoli_mat();
	/*
	 *	nctrl == 1 is CNOT
	 * nctrl == 2 is regular toffoli
	 */
	MatrixXcf toffoli_mat(int nctrl);

	/*
	 *	nctrl == 1 is CNOT
	 * nctrl == 2 is regular toffoli
	 * Identity matrix with the lower right corner == mat
	 */
	MatrixXcf generic_control_mat(int nctrl, const Matrix2cf& mat);

	Matrix2cf pauli_X_mat();
	Matrix2cf pauli_Y_mat();
	Matrix2cf pauli_Z_mat();

	Matrix2cf rot_X_mat(float theta);
	Matrix2cf rot_Y_mat(float theta);
	Matrix2cf rot_Z_mat(float theta);

	Matrix2cf phase_scale_mat(float theta);
	Matrix2cf phase_shift_mat(float theta);

	// Identity matrix with lower right corner == phase_shift_mat
	Matrix4cf control_phase_shift_mat(float theta);

	Matrix4cf swap_mat();
	Matrix<CX, 8, 8> cswap_mat();

	MatrixXcf qft_mat(int nqubit);

	MatrixXcf grover_diffuse_mat(int nqubit);
}

#endif // qumat_h__
