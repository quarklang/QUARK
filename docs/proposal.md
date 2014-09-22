QUARK
=====
#####QUantum Analysis and Realization Kit
A High Level Programming Language for Quantum Computing

*Daria Jung (djj2115), Jamis Johnson (jmj2180), Jim Fan (lf2422), Parthiban Loganathan (pl2487)*

####Introduction
In the early 1980's, Richard Feynman observed that certain quantum mechanical effects could not be efficiently simulated using classical computation methods. This led to the proposal for the idea of a "quantum computer", a computer that uses the effects of quantum mechanics, such as superposition and entanglement, to its advantage. Though quantum computing is still in relative infancy, in 1994, Peter Shor (Bell Labs) developed a quantum algorithm to factor integers in polynomial time, providing motivation and renewed interest in building quantum computers and discovering other quantum algorithms. 

Classical computers require data to be encoded in binary digits, where each bit is always in a definite state of either 0 or 1. Quantum computation uses qubits in order to represent a superposition of states. Operating on qubits effectively operates on different possible states of being at the same time. By performing a single operation on a one bit qubit, we perform operations on two different states of a qubit at once. With certain algorithms, we can use this parallelism in order to solve problems in significantly less time than a classical computer would take.

We would like to propose QUARK, a domain-specific imperative programming language to allow for expression of quantum algorithms. The purpose of QUARK is to define quantum computing algorithms in a user-friendly way and construct real quantum circuit instructions. In theory, our language could produce quantum circuits that could be run on actual quantum computers in the future. QUARK incorporates classical computing methods and control structures, which allow operations on quantum and classical data. It supports user defined operators and functions to make the esoteric notion of writing algorithms for quantum computers a reality. Built in data types like complex numbers, fractions, matrices and quantum registers combined with a robust standard library that supports Hadamard and other quantum gates make QUARK a great way to get started with quantum computing.

A basic quantum circuit simulator is included as part of the QUARK architecture. Quantum operators and data types in QUARK compile to C++, which can then be passed onto our quantum simulator.

####Syntax
######Comments
```
% single line comment
%{
  multi-line comments
}%
```

######Variable Declarations
Variables are declared in an imperative style with dynamic typing. There is no need to declare the type of the variable or demarcate a new variable with a keyword. The variable name is on the left and it is assigned a value using `=` operator to the result on the right side of the assignment. Also, every line ending is indicated by a `;` like in Java or C. We also suggest naming variables using camel case.
```
someVariable = "variable";
someOtherVariable = 10;
```

######Types
QUARK supports the following types:
- Numbers
- Fractions
- Complex Numbers
- Booleans
- Strings
- Lists
- Matrices
- Quantum Registers

######Numbers
All numbers are floats. There is no distinction between integers and floats.
```
num = 2;
pi = 3.14;
```

######Fractions
Fractions can represent arbitrary precision and are represented using a `$` sign to separate the numerator and denominator.
```
% pi represents 22/7
pi = 22$7;
1/pi; % returns 7/22
pi + 1%7; % returns 23/7
```

######Complex Numbers
Complex numbers can be represented using the notaiton `a+bi` where `a` and `b` are Numbers.
```
complex = 3+1i % This represents 3+i. We need b=1. It can't be omitted
complex * -.5i; % Arithmetic operations on complex numbers. This returns .5-1.5i
(-.5 + .3i) ** 5; % Use **n to raise to the power n
complex = 2.6 - 1.3i;
% The following assertions resturn true
norm(complex) == 2.6**2 + (-1.3)**2; % use norm() to get the norm
complex[0] == 2.6; % get real part
complex[1] == -1.3; % get imaginary part
abs(complex) == sqrt(norm(complex)); % use abs() to get the absolute value
```

######Booleans
It's simply `true` and `false`.
```
isThisAnAwesomeLanguage = true;
```

######Strings
Strings can be represented within double quotes. There are no characters. Characters are just strings of length 1. Escape a double quote with `\` as in `\"`. Get the string length with the `len()` function. Concatenate strings with `+=`. Access parts of string using `[]`.
```
someString = "Hello World";
len(someString); % returns 11
someString += "!"; % It's now "Hello World!"
someString[4]; % returns "o"
```

######Lists
We make it easy to use lists, kinf od like Python.
```
someList = [1:5]; % returns list {1, 2, 3, 4, 5};
anotherList = {"a", "b", "c", "d", "e", "f"}; % can be used to explicitly define elements in list
anotherList[-1]; % returns "f"
anotherList[2:4]; % returns {"c", "d", "e"}
len(lis); % len() returns the size of the first dimension of the list
```

######Matrices
Matrices are also easy to use. Similar to Matlab.
```
mat = [[2, 3],[5, 6],[-1, 2]];
mat'; % transpose a 2D matrix
mat2 = [[-2, 3],[0, 6]];
len(mat[0]) == 2; % returns true. This is the column dimension
len(mat) == 3; % returns true. This is the row dimension
mat[0][1]; % get element at position (0,1)
```

######Quantum Registers
We support quantum registers that take a size and .
You can query a quantum register for its stats using `?`.
```
size = 5
qr1 = <size, 1> % dense quantum register with initial state 1
qr2 = <<size, 0>> % sparse quantum register with initial state 0
qr1?1 % what does this return?
```
