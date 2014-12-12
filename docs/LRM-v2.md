
##Quark Language Reference Manual

| Name    				| UNI      | 
| :-------------------- | ------:  |
| Daria Jung 			| djj2115  | 
| Jamis Johnson    		| jmj2180  | 
| Jim Fan     			| lf2422   |
| Parthiban Loganathan  | pl2487   |  

## Table of contents

[TOC]


###Introduction

This is the language reference manual for Quark, a high level language for quantum computing. Quark makes it simple to perform complicated mathematical operations required for quantum computing in a simple and intuitive manner. In addition to standard types featured in most common programming languages, Quark supports complex numbers, fractions, matrices and quantum registers.

##Reference Manual

###Grammar Notation
FILL OUT LATER

###Lexical Conventions
A program in QUARK includes at least one function definition, though something trivial like a variable declaration or a string should compile. Programs are written using a basic source character set accepted by the C++ compiler in use. Refer to what source-code file encoding your compiler accepts. The QUARK compiler will only output ASCII.

###Line Terminators
?

###Comments
MATLAB style commenting is supported. A MATLAB style comment begins with `%` and ends with `%`. Multi-line MATLAB comments start with `%{` and end with `}%`. Any sequence of characters can appear inside of a comment except the string `}%`. These comments do not nest. 

###Whitespace
Whitespace is defined as the ASCII space, horizontal tab and form feed characters, as well as line terminators and comments.

###Tokens
Tokens in QUARK consist of identifiers, keywords, constants, and separators. Whitespace is ignored and not taken into consideration.

###Identifiers
An identifier is composed of a sequence of letters and digits, the first of which must be a letter. There is no limit on the length of an identifier. The underscore character `_` is included in the regular expression pattern for letters. 

Two identifiers are the same if they have the same ASCII character for every letter and digit. 

```
digit -> ['0'-'9']
letter -> ['a'-'z' 'A'-'Z' '_']
Identifier -> letter (letter | digit)* 
```

###Keywords
The following identifiers are reserved for use as keywords, and may not be used otherwise:

> bool
> int
> float
> fraction
> complex
> qreg
> void
> mod
> in
> return
> continue
> break
> while
> for
> else
> if
> and
> not
> or

###Punctuation
**Parenthesis** -- Expressions can include expressions inside parenthesis. Parenthesis can also indicate a function call.

**Braces** -- Braces indicate a block of statements.

**Semicolon** -- Semicolons are used at the end of every statement as a terminator. Semicolons are also used to separate rows in the matrix data type.

**Colon** -- Colons are used to denote slicing in arrays and within a function declaration. In a function declaration, formal arguments appear between the colon and a left curly brace. 

**Dollar Sign** -- The dollar sign separates the numerator value from the denominator value in a fraction data type.

**Comma** -- Commas have several use cases. Commas are used to separate formal arguments in a function declaration, elements in arrays and matrices, and the size and initial state of a `qreg`.

###Escape Sequences
Certain characters within strings need to be preceded by a backslash. These characters and the sequences to produce them in a string are:

| Character | Sequence 	|
| :--------:|:--------- |
|	\"		|	 "	   	|
|   \n		|  linefeed |
| \r		| carriage return |
| \t		| horizontal tabulation|
| \b 		| backspace |

###Data Types
The data types available in QUARK are:

> int
> float
> fraction
> bool
> complex
> string
> qreg
> matrix
> void

Additionally, the aggregate data type of array is available to the user.

####int
An `int` is a 64-bit signed integer.

####float
A `float` is a 64-bit signed floating-point number.

####fraction
A `fraction` is denoted by two `int` types separated by `$`. 

####bool
A `bool` value is denoted using the literals `true` or `false`.

####complex
A `complex` type is generated from two `int` or `float` values; if given a mix of `int` and `float` types, QUARK will implicitly type cast. A `complex` type can also be generated with one numerical value, which will be assigned to the real part of a complex number; imaginary will default to 0. The real and imaginary parts of a complex number can be accessed by `real` and `im` accessors.

```ocaml
complex cnum = i(3.0, 1);
real(cnum); % 3.0
im(cnum); % 1
complex cnum2 = i(9) % this gives us i(9, 0)
```

####string
A `string` is a sequence of characters. String literals are placed between double quotations.

####qreg
A `qreg` type represents a quantum register. A `qreg` accepts two `int` types. The left value denotes the initial size of a quantum register, and the right value denotes the initial bit.

```
qreg q = <| 1, 1 |>;
```

####matrix
QUARK allows you to create matrices; a `matrix` uses a special bracket notation to distinguish from arrays, and rows are separated by semicolons. Matrices may be composed of only `int`, `float`, or `complex`.

`[| r00, r01; r10, r11; r20, r21 |]`

####array
QUARK allows arrays of any of the above data types. Arrays are of variable length and are arbitrarily dimensional. 

Arrays can be initialized using a comma-separated list delimited by square brackets [ ]. Additionally, arrays can be declared with a size to create an array of uninitialized elements. 

Arrays may be concatenated with the `&` operator. 

```
int[5]; % gives us [0,0,0,0,0]
int[] a = [1, 2, 3]; % array initialization
int[][] b = [[1,2,3], [4,5,6]]; % 2-d array

[11, 22, 33] & int[3]
% gives us [11, 22, 33, 0, 0, 0]
```

Array indices can be accessed using the square bracket notation with an integer such as:
```
int[] arr = [0, 1, 2];
arr[0]; 
```
or
```
int[] arr = [0, 1, 2];
int i = 0;
arr[i];
```

Indices of multidimensional arrays may be accessed by separating the dimensional index numbers by commas:

```
int[][] arr = [[0,1,2],[3,4,5]]
arr[1][1]; % accesses 4
```

The built-in `len` function returns an `int` representing the length of the array.

####void
Void is a type for a function that returns normally, but does not provide a result value to the caller.

###Function types
Functions take in zero or more variables of primitive or array types and optionally return a variable of primitive or array type. A function declaration always begins with `def`, the return type of the function, a colon `:`, and a list of formal parameters which may be empty.

```
def void main: int x
{
	% statement
}
```

###Declarations

####Declaring a Variable
Variables can be defined within individual functions or in the global scope. Variables may be declared and then defined, or declared and defined simultaneously. An expression to which a value may be assigned is called an LValue.

```
int x; % definition
x = 5; % declaration
int y = 6; % definition and declaration
```
x and y are LValues. LValues are named as such because they can appear on the left side of an assignment (though they may also appear on the right side).

####Declaring an Array
As previously shown, arrays can be multidimensional, and may be of variable length. Arrays may be declared on their own with a size to get an uninitialized array of the given size. They can also be initialized with values upon declaration.

```
int[5]; % gives us [0,0,0,0,0]
int[] a = [1, 2, 3]; % array initialization
int[][] b = [[1,2,3], [4,5,6]]; % 2-d array
```

####Declaring a Matrix



###Operators

###Iterators
for i in [1:10]
for int i in [1:10]
for int i in array_of_int


