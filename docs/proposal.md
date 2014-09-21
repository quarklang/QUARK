Proposal
========

###QUARK: A High Level Programming Language for Quantum Computing
---

*Daria Jung (djj2115@columbia.edu), Jamis Johnson (jmj2180@columbia.edu), Jim Fan (lf2422@columbia.edu), Parthiban Loganathan (pl2487@columbia.edu)*

#####Introduction

In the early 1980's, Richard Feynman observed that certain quantum mechanical effects could not be efficiently simulated using classical computation methods. This led to the proposal for the idea of a "quantum computer", a computer that uses the effects of quantum mechanics, such as superposition and entanglement, to its advantage. Though quantum computing is still in relative infancy, in 1994, Peter Shor (Bell Labs) developed a quantum algorithm to factor integers in polynomial time, providing motivation and renewed interest in building quantum computers and discovering other quantum algorithms. 

Classic computers require data to be encoded in binary digits, where each bit is always in a definite state of either 0 or 1. Quantum computation uses qubits in order to represent a superposition of states. Operating on qubits effectively operates on different possible states of being at the same time. By performing a single operation on a one bit qubit, we perform operations on two different states of a qubit at once. With certain algorithms, we can use this parallelism in order to solve problems in significantly less time than a classical computer would take.

We would like to propose QUARK, a domain-specific imperative programming language to allow for expression of quantum algorithms. The purpose of QUARK is to simulate quantum computing in a user-friendly way, and possibly compile to real quantum circuit instructions if we have a physical quantum computer some time in the future. 

Quantum computers, like classical computers, consists of three parts, a memory component which holds the current state of the machine, a processor, which performs operations on the machine state, and an input/output to set the initial state and final state of computation. Since quantum machines must currently be controlled by classical devices, QUARK incorporates classic computing methods and control structures, which allow operations on quantum and classical data. A basic quantum circuit simulator is included as part of the QUARK architecture.

#####Built In Data Types
- Integers
- Floats
- Fractions
- Strings
- Complex numbers
- Matrices
- Quantum registers

QBits and quantum gates are encapsulated in the built-in function calls. 

ADD JIM'S CODE SNIPPETS