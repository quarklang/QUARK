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
The following identifiers are reserved
```
num complex frac bool string had had_multi if else while return for in len bit
```

#####Constants
######Number Constants
Numbers are represented as either a sequence of digits or an integer part, a decimal point, a fraction part, and an 
optionally-signed exponent part which consists of an 'e' and a sequence of integers. If the decimal point and the 
exponent part are included then the fraction part is necessary. All numbers are considerred as floats.

######String Constants
Strings can one or more string constants enclosed in double or single quotes. Individual
string constants can be alphabetical characters - both lower and uppper case - and 
special reserved escape sequences which are composed of a 
backslash `\` followed by an alphabetical character. The following escape sequences are defined:

- `\\\\`
- `\n`
- `\'`
- `\"`
- `\t`
- `\r`


#####Comments
#####Comments
#####Comments
