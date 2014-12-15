Quark
-----
#####The Quantum Analysis and Realization Kit Language

| Name    		| UNI       | 
| :-------------------- | :-------: |
| Daria Jung 		| djj2115   | 
| Jamis Johnson    	| jmj2180   | 
| Jim Fan     		| lf2422    |
| Parthiban Loganathan  | pl2487    |  

December 14 2014

Contents
========
- Introduction
- Tutorial
- Language Reference Manual
- Project Plan
- Architecture
- Testing
- Lessons Learned
- Appendix

Introduction
============
###History
In the early 1980's, Richard Feynman observed that certain quantum mechanical effects could not be efficiently simulated using classical computation methods. This led to the proposal for the idea of a "quantum computer", a computer that uses the effects of quantum mechanics, such as superposition and entanglement, to its advantage. 

Classical computers require data to be encoded in binary bits, where each unit is always in a definite state of either 0 or 1. Quantum computation uses qubits, a special unit that can be 0 and 1 at the same time, i.e. a superposition of base states. Measuring a qubit will force it to collapse to either 0 or 1, with a probability distribution determined by its amplitude. 

Qubits effectively operates on exponentially large number of entangled states simultaneously, though all of them will collapse as soon as we make a measurement. With carefully designed quantum algorithms, we are able to speed up certain classical problems dramatically by tapping into such massive computational resource. It is not unlike parallel computing, but powered by quantum mechanical laws. 

Though quantum computing is still in its infancy, the last two decades have witnessed two ingenious algorithms that produced much inspiration and motivation for quantum computing research. One is Shor's algorithm (1994) for integer factorization, which yields exponential speedup over the best classical alternative, and the other is Grover's search algorithm (1996), which provides quadratic speedup for unsorted database search. Once realized, the former would have significant impact on cryptography, while the latter would have great implication on NP-hard problems. 

###Language
Quark is a domain-specific imperative programming language to allow for expression of quantum algorithms. The purpose of QUARK is to ease the burden of writing quantum computing algorithms and describing quantum circuits in a user-friendly way. In theory, our language can produce quantum circuit instructions that are able to run on actual quantum computers in the future. 

Most quantum algorithms can be decomposed into a quantum circuit part and a classical pre/post-processing part. Recognizing this, QUARK is designed to integrate classical and quantum data types and controls in a seamless workflow. Built in types like complex numbers, fractions, matrices and quantum registers combined with a robust built-in gate library make QUARK a great starting point for quantum computing researchers and enthusiasts.

A relatively efficient quantum circuit simulator is included as part of the QUARK architecture. Source code written in QUARK is compiled to C++, which can then be passed onto our quantum simulator.

Tutorial
========
Language Reference Manual
=========================

Project Plan
============
Tools used:
- Trello for task assignment
- Git for version control
- GitHub for code management
- Vagrant with Ubuntu 12.04 64-bit for consistent develoment environments

We also used an external simulator that Jim created over the summer. Our compiler's
output is C++ specifically designed to work with the simulator.

#####Project Timeline:
| Date     | Milestones                                   |
| -------- |:--------------------------------------------:|
| 9/8/14   | Team formed                                  |
| 9/9/14   | Set up dev environment and GitHub repository |
| 9/10/14  | Decided on language specifics                |
| 9/17/14  | Assigned team roles                          |
| 9/24/14  | Language proposal complete                   |
| 10/26/14 | First draft of Language Reference Manual     |
| 11/10/14 | Basic scanner and parser complete            |
| 11/21/14 | Scanner and parser complete                  |
| 11/28/14 | Semantic checking started                    |
| 12/5/14  | Semantic checking complete                   |
| 12/7/14  | Codegen complete                             |
| 12/8/14  | Test suite complete                          |
| 12/9/14  | End-to-end working                           |
| 12/10/14 | Modifications to simulator for compatibility |
| 12/13/14 | Rewrote Language Reference Manual            |
| 12/15/14 | Project report complete                      |

#####Roles and Responsibilities

Here are our official roles for the project.

| Role                      | Name                  | 
| ------------------------- | :-------------------: |
| Project Manager           | Parthiban Loganathan  |
| Language Guru             | Jim Fan               |
| System Architect          | Jamis Johnson         |
| Verification & Validation | Daria Jung            |

In practice, we didn't follow these roles very strictly. All of us worked on multiple parts of the code
and took responsibility for whatever we touched. The parts of the compiler and project that we primarily worked on can roughly be split up into the following:

| Category                  | Names                                           | 
| ------------------------- | :---------------------------------------------: |
| Project Management        | Parthiban Loganathan                            |
| Language Reference Manual | Daria Jung                                      |
| Scanner, Parser           | Parthiban Loganathan, Daria Jung                |
| Semantic Checking         | Jamis Johnson, Jim Fan                          |
| Code Generation           | Jim Fan                                         |
| Testing                   | Daria Jung, Parthiban Loganathan, Jamis Johnson |
| Simulator                 | Jim Fan                                         |
| Project Report            | All                                             |

Due to our decision to follow the "democracy" approach as opposed to the "dictatorship" approach
we faced issues with accountability, but each one of us also got to see more of the compiler in the
process.


Architecture
============


Testing
=======

We created a set of test scripts in quark (with extension .qk) and expected output text files (with extension .output). The test suite script testall.sh compiles all test scripts and runs the output C++ using the simulator. If the output matches the corresponding expected output from the .output file, the test succeeds, else fails.

We tested each significant individual component of the language from the LRM with a separate test.


Lessons Learned
===============
1. We should have heeded the numerous numerous at the beginning of the class that we should start early. While we made good initial progress with team formation, setting up our environment and drafting a proposal, we failed to actually start working on the compiler till around the midterm - partly due to insufficient knowledge of how a compiler works. Slow and steady progress would have been much less stressful than working a large number of hours in the past few weeks.
2. Project management is hard. It was difficult to get everyone to meet periodically to discuss progress and language design. Unlike a company, where your primary responsibility is to be developing software, as students with other classes and responsibilities, the project was not a priority till the end of the semester.
3. Allocating work into sizable chunks was a challenge due to the interrelatedness of the different components. Even after defining interfaces, we often found minor specification differences between the parser and code generation led to issues.
4. Focus on the primary purpose of the language. We initially toyed with the idea of dynamic typing and other advanced features that did not come to fruition.
