Proposal
========

###QUARK: A High Level Programming Language for Quantum Computing
---

*Daria Jung (djj2115@columbia.edu), Jamis Johnson (jmj2180@columbia.edu), Jim Fan (lf2422@columbia.edu), Parthiban Loganathan (pl2487@columbia.edu)*

#####Introduction

In the early 1980's, Richard Feynman observed that certain quantum mechanical effects could not be efficiently simulated using classical computation methods. This led to the proposal for the idea of a "quantum computer", a computer that uses the effects of quantum mechanics, such as superposition and entanglement, to its advantage. Until recently, the field of quantum computing remained in infancy. In 1994, Peter Shor (Bell Labs) developed a quantum algorithm to factor integers in polynomial time, providing motivation and renewed interest in building quantum computers and discovering other quantum algorithms. 

We would like to propose QUARK, a domain-specific imperative programming language to allow for expression of quantum algorithms. The purpose of QUARK is to simulate quantum computing in a user-friendly way, and possibly compile to real quantum circuit instructions if we have a physical quantum computer some time in the future. 

Quantum computers, like classical computers, consists of three parts, a memory component which holds the current state of the machine, a processor, which performs operations on the machine state, and an input/output to set the initial state and final state of computation. QUARK, in addition to classic computing methods, supports qubits and quantum registers, unitary operators, register operators, and quantum gates. This allows for the construction of quantum algorithms, which consist of unitary transformation and measurement of the resulting state. A basic quantum circuit simulator is included as part of the QUARK architecture.