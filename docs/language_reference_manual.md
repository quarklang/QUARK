##QUark Language Reference Manual

*Daria Jung (djj2115), Jamis Johnson (jmj2180), Jim Fan (lf2422), Parthiban Loganathan (pl2487)*

####Introduction

This is the reference manual for QUark, a high level language for quantum computing. 

####Lexical Conventions

#####Comments
Single line comments are denoted using a `%` while multi-line comments use `%{  }%`. Anything 
between the brackets will be commented out.


#####Identifiers
Identifiers are made up of alphabetical characters, numbers, underscores, and the first character cannot be a number.
Identifiers are case sensitive.

#####Keywords
The following identifiers are reserved:

```
qreg num complex frac bool str if elif else while return for in len bit and or null
```


#####Constants
######Number Constants
Numbers are represented as either a sequence of digits or an integer part, a decimal point, a fraction part, and an 
optionally-signed exponent part which consists of an 'e' and a sequence of integers. If the decimal point and the 
exponent part are included then the fraction part is necessary. All numbers are considerred as floats and will be 
compiled down to c++'s 8-byte, double precision type.

######String Constants
Strings can one or more string constants enclosed in double or single quotes. Individual
string constants can be alphabetical characters - both lower and uppper case - and 
special reserved escape sequences which are composed of a backslash `\` followed 
by an alphabetical character. The following escape sequences are defined:

- `\\`
- `\n`
- `\'`
- `\"`
- `\t`
- `\r`


#####Syntax Notation
In this definition we will use **bold** to define literals and *italics* for categories. 
We use Backus-Naur Form to speficy the grammar.

######Types

*type-specifier ::= primitive-type | array-type | function-type | null*

Identifiers have an associated type and the null type has no value.

######Primitive Types

*primitive-type ::= number-type | fraction-type | complex-type | quantum-register-type | boolean-type | string-type*

######Number Type

Numbers are denoted using the following the literal **num**

All numbers will be compiled to c++ doubles.

######Fraction Type

Fractions are given by the following literal *frac* and can be constructed using 
the syntax

*fraction-type ::= number-type $ number-type*

######Complex Type

*complex* is the literal used to denote the complex type and is composed of numbers having the form:

*complex-type ::= number +/- number i*

The real and imaginary parts can be accessed using `re` and `im`.

######Quantum Register Type

There are two quantum register types: sparse and dense. The bracket literals, `<` and `>` are used 
to denote a quantum register and an optional apostrophe suffix, `'` means the quantum register is 
treated as sparse.

*quantum-type ::== <number,number> | <number,number>'*

The first number is the size of the quantum register and the right number is the initial state.

######Boolean Type

Booleans use the literal *bool* and can take the value of the literals `true` or `false`.

######String Type

We use the **str** literal to indicate a string type.

######List Type

*list-type :== [primitive-type]*
