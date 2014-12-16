Quark
-----
#####The Quantum Analysis and Realization Kit Language

| Name          | UNI       | 
| :-------------------- | :-------: |
| Daria Jung        | djj2115   | 
| Jamis Johnson     | jmj2180   | 
| Jim Fan           | lf2422    |
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

###Install
Install [Vagrant](https://www.vagrantup.com/downloads.html), a tool for provisioning
virtual machines and used to maintain a consistent environment. You will also need 
to install [VirtualBox](https://www.virtualbox.org/wiki/Downloads) which Vagrant 
uses for virtualization. Git clone the (Quark repository)[https://github.com/quarklang/QUARK],
navigate to the directory, and run `vagrant up`. This will provision and run a 
Ubuntu 14.04 LTS virtual machine instance as well as download and install dependencies 
such as OCaml and g++-4.8. Run `vagrant ssh` to ssh into the vm. Make sure you 
are in the `/vagrant` directory by running the command `pwd` and if you are not run 
`cd /vagrant`.

###Compiling and Running Quark Programs
The following Hello World example Quark program is saved in `/tests/hello_world.qk`.

```
def int main:
{
    print("hello world");

    return 0;
}
```

Before we can compile Quark programs into C++ we must build the Quark compiler `quarkc`.
Navigate to `/vagrant/quark/` and run `make`. Ensure `quarkc` was properly built by 
checking for error messages then running `./quarkc -h`  to see compilation options. 
You should see the following:

```
usage: quarkc -s source.qk [-c output.cpp ] [-o executable] [-static] [-g++ /path/to/g++]
  -s : quark source file
  -c : generated C++ file. If unspecified, print generated code to stdout
  -o : compile to executable. Requires g++ (version >= 4.8)
  -sco : shorthand for -s <file>.qk -c <file>.cpp -o <file>
  -sc : shorthand for -s <file>.qk -c <file>.cpp
  -g++ : shorthand for -s <file>.qk -c <file>.cpp
  -static : compile with static lib (otherwise with dynamic lib). Does NOT work on Mac
  -help  Display this list of options
  --help  Display this list of options
```

As stated above, to compile `tests/hello_world.qk` into C++ and an executable run
`./quark/quarkc -s tests/hello_world.qk -c hello_world.cpp -o hello_world`. 
You can run the hello_world executable and see the generated C++ as follows:

```
vagrant@vagrant-ubuntu-trusty-64:/vagrant$ ./hello_world
hello world

vagrant@vagrant-ubuntu-trusty-64:/vagrant$ cat hello_world.cpp
#include "qureg.h"
#include "qumat.h"
#include "qugate.h"
#include "quarklang.h"

using namespace Qumat;
using namespace Qugate;

int main()
{
std::cout << std::boolalpha << std::setprecision(6) << std::string("hello world") << std::endl;
return 0;
} // end main()
```

The C++ includes are referencing our quantum simulator and these files can be found 
in the `lib` directory.

###More Examples
Quark sytax resembles something along the lines of Python with static typing and 
if you already know an imperative language such as C, Python or Go you should be 
able to easily glean the majority of the syntax by simply reading through these 
examples. For a more in-depth explanation of Quark syntax see the language manual.


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
These are goals we set for our project.

| Date     | Goal                           |
| -------- |:------------------------------:|
| 11/21/14 | Complete scanner and parser    |
| 12/5/14  | Complete semantic checking     |
| 12/7/14  | Complete code generation       |
| 12/8/14  | Complete test suite            |
| 12/1/14  | Complete end-to-end            |
| 12/5/14  | Finish testing and code freeze |
| 12/8/14  | Complete project report        |

#####Project Log:
Actual progress of project.

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
| 12/7/14  | Code generation complete                     |
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


Test Plan
=======

The test suite for QUARK consisted of simple regression tests for language features, as well as longer tests to demonstrate target programs in the language.

We created a set of test scripts in quark (with extension .qk) and expected output text files (with extension .output). The test suite script testall.sh compiles all test scripts and runs the output C++ using the simulator. If the output matches the corresponding expected output (.coutput) from the .output file, the test succeeds, else fails.

We tested each significant individual component of the language from the LRM with a separate test.

###Rationale
We chose not to include unit testing and relied on OCaml's type system to detect major bugs or give us warnings about things such as missing cases in a pattern match.

Tests were run frequently to detect changes or unimplemented features. The regression test system allowed us to utilize test-driven development. The test suite was run with tests that used unimplemented language features, simply failing until those features are implemented.

###Implementation 
We created a `tests` folder in the Quark source repository and a shell script `testall.sh` to implement the build procedure. 

To run tests, in the top level directory of the Quark repository, run `./testall.sh` to:

1. Using the QUARK compiler quarkc, compile all programs with the extension `.qk` in `tests` to C++ files. 
2. Using `g++` 4.8, compile the generated C++ files to executables linked with the QUARK simulator libraries.
3. Run the executables and compare the outputs (`.coutput`) to the expected output files (`.output`).  Any files with differing output are considered to be failing tests.

Add a `.qk` file to the `tests` folder with a corresponding `.output` file to add a new test. 

###Representative Program
Below is our implementation of Grover's search, a quantum algorithm for searching 
an unsorted database in O(N<sup>1/2</sup>) time and a non-trivial example of a 
Quark program.

```
int M = 221;

def int gcd: int a, int b
{
    int c;
    while a != 0:
    {
        c = a;
        a = b mod a;
        b = c;
    }
    return b;
}

def int exp_mod: int b, int e, int m
{
    int remainder;
    int x = 1;

    while e != 0:
    {
        remainder = e mod 2;
        e = e >> 1;

        if remainder == 1:
            x = (x * b) mod m;
        b = (b * b) mod m;
    }
    return x;
}

def int smallest_period: int b, int M
{
    int i = 1;
    while exp_mod(b, i, M) != 1:
        i ++;
    return i;
}

def int long_pow: int a, int p
{
    if p == 1:
        return a;
    int partial = long_pow(a, p / 2);
    if p mod 2 == 1:
        return partial * partial * a;
    else
        return partial * partial;
}

def int log2_int: int x
{
    int ans = 0;
    while x > 0:
    {
        x = x >> 1;
        ans ++;
    }
    return ans;
}

%{
    If size == 0, continue until 0
}%
def int[] to_continued_fraction: fraction frac, int size
{
    int[] cfrac;
    int i = 0;
    while size < 1 or i < size:
    {
        % array concatenation
        cfrac &= [num(frac) / denom(frac)];
        frac -= cfrac[i];
        if num(frac) == 0 : break;

        % denom/num built-in
        frac = ~frac;
        i ++;
    }
    return cfrac;
}

def fraction to_fraction: int[] cfrac, int size
{
    if size < 1:
        size = len(cfrac);
    fraction ans = 1$cfrac[size - 1];
    for int i in [size-2 :0 :-1] :
    {
        ans += cfrac[i];
        ans = ~ans;
    }
    return ans + cfrac[0];
}

int nbit = log2_int(M) + 1;

% This is the user defined function that should be passed as a string argument
def int shor_oracle: int x
{
    return exp_mod(nbit, x, M);
}

def int main:
{
    qreg q0 = <| nbit * 2, 0 |>;

    qft(q0, 0, nbit);

    int b; int i;
    while true:
    {
        b = rand_int(2, M);

        if gcd(b, M) != 1: continue;

        qreg q = qclone(q0);

        apply_oracle(q, "shor_oracle", nbit);

        qft(q, 0, nbit);

        int mTrial = 0;
        int measured;

        while mTrial < 10:
        {
            mTrial ++;
            measured = q ?' [:nbit];
            if measured != 0:
            {
                int[] cfrac = to_continued_fraction((1 << nbit)$measured, 0);
                for int size in [len(cfrac):0:-1] :
                {
                    int p = num(to_fraction(cfrac, size));
                    int P = p;

                    while P < 128 and P < M :
                    {
                        if P mod 2 == 0
                            and exp_mod(b, P, M) == 1 :
                        {
                            int check = exp_mod(b, P / 2, M);
                            if check != 1 and check != M - 1 :
                            {
                                int b_P_1 = long_pow(b, P / 2) - 1;
                                int prime = gcd(M, b_P_1);

                                if prime != 1 and prime != -1 :
                                {
                                    print("Found period r = ", P);
                                    print("b ^ r = ", b, " ^ ", P, " = 1 mod ", M);
                                    print("b ^ (r/2) = ", b, " ^ ", P / 2, " = ", check, " mod ", M);
                                    int prime2 = gcd(M, b_P_1 + 2);
                                    print("gcd(", M, ", ", b_P_1, ") = ", prime);
                                    print("gcd(", M, ", ", b_P_1 + 2, ") = ", prime2);
                                    int other_prime;
                                    if prime2 == 1 :
                                        other_prime = M / prime;
                                    else
                                        other_prime = prime2;
                                    print("\nFactorize ", M, " = ", prime, " * ", other_prime);
                                    return 0;
                                }
                            }
                        }
                        P += p;
                    }
                }
            }
        }
    }
    print("FAIL");
    return 0;
}
```

###Tests Used

| Test name | Purpose |
| :-------- | :------- |
| gcd.qk |  ensures QUARK passes the GCD test
| test-addition.qk | ensures integer arithmetic works
| test-array.qk | ensures that arrays can both be written to and read from
| test-complex.qk | ensures QUARK's support for complex numbers works correctly 
| test-hello_world.qk | ensures basic print functionality works 
| test-import.qk | ensures QUARK's import system can correctly access code in another `.qk` file
| test-logic.qk | ensures boolean logic works correctly 
| test-matrix.qk | ensures matrices can both be written to and read from 
| test-range.qk | ensures range iteration works correctly
| test-while.qk | ensures while loops correctly execute based on condition
| test-float.qk | ensures float arithmetic works
| test-elif.qk | ensures else-if works correctly
| test-multi-array.qk | ensures we are able to access and write to multi-dimensional arrays

Lessons Learned
===============

#####Parthiban Loganathan
1. We should have heeded the numerous numerous at the beginning of the class that we should start early. While we made good initial progress with team formation, setting up our environment and drafting a proposal, we failed to actually start working on the compiler till around the midterm - partly due to insufficient knowledge of how a compiler works. Slow and steady progress would have been much less stressful than working a large number of hours in the past few weeks.
2. Project management is hard. It was difficult to get everyone to meet periodically to discuss progress and language design. Unlike a company, where your primary responsibility is to be developing software, as students with other classes and responsibilities, the project was not a priority till the end of the semester.
3. Allocating work into sizable chunks was a challenge due to the interrelatedness of the different components. Even after defining interfaces, we often found minor specification differences between the parser and code generation led to issues.
4. Focus on the primary purpose of the language. We initially toyed with the idea of dynamic typing and other advanced features that did not come to fruition.

#####Daria Jung
Group projects are pretty frustrating when the group is comprised of several overworked university students. Real life can get in the way (one group member experienced a death in the family), things ALWAYS take longer than expected (programmers are the worst at estimating how long something will take), and writing a compiler can get pretty complicated. Communication, or lack thereof, was an impediment to our progress when we started to get going, so it is imperative to be transparent and crystal clear to other teammates about what is happening. I wish that we had had Bob Martin's talk earlier in the semester so we had a better sense of the sorts of things to watch out for. The pace of the project was much different than working on something at a company.

Definitely start the project as early as you can, which I'm sure most people have said, or agree with. Things inevitably start to pile up (job interviews, school, midterms, personal issues), and if you have buffer time, then things won't be as hectic in the last few weeks of the semester. 

It's pretty difficult to delegate/divide up work, so I would have liked to pair program more. Inevitably, some of the work fell on certain people throughout the project due to the nature of everyone's different schedules.

#####Jamis Johnson
Communication is vital. Pick a time to meet everyweek for an hour or two and don't leave without knowing specifically who is doing what before next week's meeting. Decide immediately how you will all communicate (email, sms, facebook messanger) and constantly inform your teammates of your progress!

Jump in and start coding stat! The language spec changes rapidly so don't dwell on the minutiae. What matters most is the core objective of your language. Complete the first edition of the LRM by your team's first or second meeting and move on to writing code.

Team members will inevitably become overwhelmed with life, other school work, and with each other, and it's easy to get frusterated. Help each other out and go easy on one another (and see above: communicate!). Also, early progress will genrally ameliorate stress, as will clearly defined roles.
