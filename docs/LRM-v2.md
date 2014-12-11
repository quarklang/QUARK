
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

**Dollar Sign** -- Dollar sign separates the numerator value from the denominator value in a fraction data type.

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
An integer is a 64-bit signed integer.

####float
A float is a 64-bit signed floating-point number.

####fraction
A fraction is denoted by a numerical number (numerator) separated by `$` and another numerical number (denominator).

####complex


####bool
A boolean value is denoted using the literals `true` or `false`.

####string
A string is a sequence of characters. String literals are placed between double quotations.

####matrix
QUARK allows you to create matrices; a matrix uses a special bracket notation to distinguish from arrays, and rows are separated by semicolons. Matrices may be composed of only `int`, `float`, or `complex`.

`[| r00, r01; r10, r11; r20, r21 |]`

####array


###Iterators
for i in [1:10]
for int i in [1:10]
for int i in array_of_int


