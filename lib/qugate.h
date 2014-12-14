#ifndef qugate_h__
#define qugate_h__

#include "qureg.h"

namespace Qugate
{
	void generic_gate(Q, const Matrix2cf&, int tar);

	void generic_gate(Q, const Matrix4cf&, int tar1, int tar2);
	/*
	 *	Works with arbitrary number of target qubits
	 */
	void generic_gate(Q, const MatrixXcf&, vector<int>& tars);

	///////************** Single-qubit gates **************///////
	void hadamard(Q, int tar);
	void hadamard(Q);
	void hadamard_top(Q, int topSize);

	void pauli_X(Q, int tar);
	void pauli_Y(Q, int tar);
	void pauli_Z(Q, int tar);

	void rot_X(Q, float theta, int tar);
	void rot_Y(Q, float theta, int tar);
	void rot_Z(Q, float theta, int tar);

	void phase_scale(Q, float theta, int tar);
	void phase_shift(Q, float theta, int tar);

	///////************** Controlled gates **************///////
	void generic_control(Q, const Matrix2cf&, int ctrl, int tar);
	void cnot(Q, int ctrl, int tar);

	void generic_toffoli(Q, const Matrix2cf&, int ctrl1, int ctrl2, int tar);
	void toffoli(Q, int ctrl1, int ctrl2, int tar);

	void generic_ncontrol(Q, const Matrix2cf&, vector<int>& ctrls, int tar);
	void ncnot(Q, vector<int>& ctrls, int tar);

	void control_phase_shift(Q, float theta, int ctrl, int tar);

	///////************** Swap gates **************///////
	void swap(Q, int tar1, int tar2);

	void cswap(Q, int ctrl, int tar1, int tar2);

	///////************** Special gates **************///////
	// tarSize: number of qubits to be operated on
	void qft(Q, int tarStart, int tarSize);
	inline void qft(Q) { qft(q, 0, q.nqubit); }

	// diag([2; 0; 0; ...; 0]) - I, invert amplitude unless the state is 0^n 
	void grover_diffuse(Q, int tarStart, int tarSize);
	inline void grover_diffuse(Q) { grover_diffuse(q, 0, q.nqubit); }
}

#endif // qugate_h__
