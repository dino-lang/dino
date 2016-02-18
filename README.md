# Programming language Dino and its implementation
## Vladimir Makarov, vmakarov@gcc.gnu.org
## Feb, 2016

# Description Layout
* Introduction to Dino
    * History
    * Dino as a high-level scripting language
    * Dino as a functional language
    * Dino as an object-oriented language
* Dino Implementation
    * Overall Structure
        * Used implementation tools
        * Byte code compiler (optimizations)
        * Byte code Interpreter (GC, Concurrency, REPL)
        * JIT, Type inference
  * Performance comparison with Python, PyPy, Ruby, JS, Scala, OCAML
    on x86-64, AARH64, ARM, PPC64.

---

# Some history
* 1993: Original language design and implementation ![Dino logo](Dino.jpg "Dino logo")
    * Was used in russian computer game company ANIMATEK as a simple
      scripting language for describing dinosaurus movements
* 1998, 2002, 2007 : Major language and implementation revisions
* The next major release is planed to be done in spring of 2016.
  * This document describes the current state of Dino language
    and implementation. The document will be changed as the current
    Dino state is changing.

# The first taste of Dino
  * Eratosthenes sieve:
```
    var i, prime, k, count = 0, SieveSize = 8191, flags = [SieveSize : 1];
    for (i = 0; i < SieveSize; i++)
      if (flags[i]) {
        prime = i + i + 3;
        k = i + prime;
        for (;;) {
          if (k >= SieveSize)
            break;
          flags[k] = 0;
          k += prime;
        }
        count++;
      }
    putln (count);
```
    
---

# DINO as a scripting language
* Dino aims to look like C language
* High-Level scripting object-oriented language:
    * Multi-precision integers
    * Heterogeneous extensible arrays, array slices
    * Associative tables with possibility to delete elements
    * Powerful and safe class composition operation for (multiple)
      inheritance and traits description
    * First class functions, classes, and threads with closures,
      anonymous functions, classes, threads
    * Exception handling
    * Concurrency
    * Pattern matching
    * Unicode 8 support


---

# Arrays and Tables
* Associative tables
    * elements can be added and deleted
    * elements can be any values, e.g. other tables
    * Implemented as hash tables without buckets for compactness
      and data locality
        * Secondary hash for conflict resolutions
        * Murmur hash function for most values

---

# Array Slices
* Eratosthenes sieve with slices:
```
    var i, prime, count = 0, SieveSize = 8191, flags = [SieveSize : 1];
    for (i = 0; i < SieveSize; i++)
      if (flags[i]) {
        prime = i + i + 3;
        flags[i + prime:SieveSize:prime] = 0;
        count++;
      }
    putln (count);
```

---

# Functions
* Example:                                                                                                                                                                           
```
    fun even;
    fun odd  (i) {i == 0 ? 0 : even (i - 1);}
    fun even (i) {i == 0 ? 1 : odd (i - 1);}
    putln (odd (1000000));
```
* Anonymous functions:
```
    filter (fun (a) {a > 0;}, v);
    fold (fun (a, b) {a * b;}, v, 1);
```
* Function closures:
```
    fun incr (base) {fun (incr) {base + incr;}}
```

---

# Threads
* Thread: a function with concurrent execution
```
    thread t (n) {for (var i = 0; i < n; i++) putln (i);}
    t(100); // the following code don't wait for t finish
    for (var i = 0; i < 1000; i++) putln (“main”, i);
```
* Implemented as green threads:
    * Much faster than OS threads with Global Interpreter Lock (Python/Ruby)
    * Deterministic behaviour and automatic deadlock recognition
    * Plans to implement through OS threads without GIL for parallelism
* There is a low level sync-statement wait
```
    wait (cond) [stmt];
```
* Simple statements are atomic


---

# Object orientation
* Class is just a special type of function:
    * Returns closure, public visibility by default
```
      class num (i) {fun print {put (i);}}
      class binop (l, r) { fun print_op;
        fun print {l.print(); print_op (); r.print ();}
      }
```
* Special class/function composition:
    * emulates (multiple) inheritance, traits, and dynamic dispatching
```
      class add (l, r) {
        use binop former l, r later print_op;
        fun print_op {put (“ + “);}
      }
```

---

# Object orientation -- continuation
  * A safe and powerful way to support object orientation
      * Declarations of class mentioned in **use** are inlayed
      * Declarations before the use rewrite corresponding inlayed
        declarations mentioned in **former**-clause
      * Declarations after the use rewrite corresponding inserted
        declarations mentioned in **later**-clause
      * The declarations should be **matched**
      * The original and new declarations should be **present**
        if they are in former- or later-clause
      * The original declaration can be **renamed** and still used
        instead of just rewriting if necessary

---

# Object orientation -- continuation
* Special function **isa** to check subtyping of class or object:
```
    isa (add, binop);
    isa (add (num (1), num (2)), binop);
```
* Optimization for removing code duplication
* Syntactic sugar for a singleton object (it is implemented as
  anonymous class and corresponding object creation)

```
    obj int_pair {
      val min = 0, max = 10;
    }
```

---

# Object orientation -- continuation
* Optimizations for access to object members permit to use
  objects as **name spaces**
 
```
    obj sorts {
      var compare_fun;
      fun quick_sort (...) {...}
      fun heap_sort (...) {...}
    }
    ...
    sorts.fft (...);
```
* To make access to object more brief an **expose**-clause exists
* Exposing an object member
```
    expose sorts.quick_sort;
    quick_sort (...);
```
* You can have a member access with a different name
```
   expose sorts.quick_sort (asort);
   asort (...);
```
* You can expose all public declarations of an object
```
   expose sorts.*;
   compare_fun = ...; quick_sort (...); heap_sort (...);
```

---

# Standard spaces

* Dino has 5 standard spaces (singleton objects) avalaible by default
  to any Dino program
  * `lang` -- provides interface to fundamental Dino features
  * `io` -- provides interface for input/output and to work with file system
  * `sys` -- provides interace to some features of underlying OS
  * `math` -- mostly provides some mathematical functions
  * `re` -- provides regular expression matching functions
  * `yaep` -- provides interface to Yet Another Earley Parser
* All items in spaces `lang` and `io` are always exposed
* If you redefine some exposed item, you still can have access to it
  as a member of the space.

---

# Pattern matching
* Pattern can be
  * pattern matching anything `_`
  * pattern variable matching any value, e.g. `a`
    * the value is assigned to the variable
  * **vector pattern** matching vectors, e.g. `[v, _, 5, ...]`
  * **table pattern** matching tables, e.g. `tab ["a": v, ...]`
  * **object pattern** matching objects of given class or
    its derived class, e.g. `node (a, 10)`
  * `...` in a pattern list matching zero or more values
  * expression which looks different from the above and matches the equal value
  * pattern can be nested, e.g. `node ([v, ...], 10)`
* Pattern matching in variable declaration, e.g. `var [a, b, ...] = v;`
  * `v` should be a vector with at least 2 elements, new declared variables
    `a` and `b` will hold values of the two first elements

---

# Pattern matching -- pmatch statement
```
    pmatch (v) {
      case [...]: putln ("array"); continue;
      case [a, ...]: if (a == 0) break; putln ("array with non-zero 1st element");
      case node (v) if v != 0: putln ("object of class node with nozero parameter");
      case _: putln ("any but array");
    }
```
  * Try to match value with case patterns in a particular order,
    execute the corresponding code for the first matched pattern
  * Scope of pattern variables is the corresponding case
  * Continue means continue to try subsequent patterns
  * Break means finish the match statement execution
    * There is an implicit break at the end of each case

---


# Example: classes and functions with pattern matching
* Simple binary tree and its check:
```
    class tree {}
    class leaf (i) {use tree;}
    class node (l, r) {use tree;}
```
```
    fun exists_leaf (test, t) {
      pmatch (t) {
        case leaf (v): return test (v);
        case node (l, r):
          return exists_leaf (test, l) || exists_leaf (test, r);
      }
    }
```
```
    fun has_odd_leaf (t) {
      exists_leaf (fun (n) {type (n) == int && n % 2 == 1;}, t);
    }
```

# Regular expression matching -- rmatch statement
```
    rmatch (str) {
      case "[a-zA-Z]+": putln ("word starting at ", m[0]);
      case "[0-9]+": putln ("number starting at ", m[0]);
      case _: putln ("anything else, m is undefined");
    }
```
  * Try to match string with case regular expressions in a particular order,
    execute the corresponding code for the first matched regular expression
  * Implicitly declared variable `m` contains integer vector
    describing successfully matched sub-string
  * Scope of variable `m` is the corresponding case
  * Continue and break statements behave the same way as
    in pattern match statement

---

# Exception handling
  * Exceptions are objects of sub-classes of class **except**
    * Exceptions can be generated by Dino interpreter, e.g. floating point exception
    * or by *throw*-statement:
```
          class my_except (msg) {use except;}
          throw my_except ("my special exceptions");
```
  * Exceptions can be processed by *try*-block or *try*-operator
    * Exceptions are propagated to previous blocks on the block stack until they are processed
    * Unprocessed exceptions finish the program execution

# Exception handling -- continuation
  * *Try-block*
    * Exception occurring inside the block is processed in the first catch-block
      whose class mentioned in the catch clauses is a super-class of the processed exception class
    * The processed exception is in variable *e* implicitly defined in
      the corresponding catch-block
    * If there is no matched catch-block, the exception is propagated further
```
          try {
            var ln;
            for (;;) {
              var ln = getln (); putln (ln);
            }
          } catch (eof) { putln ("end of file"); }
```
  * *Try-operator*
    * The operator returns non-zero if no exceptions occurs in the statement given as the first argument
    * The operator returns zero if an exception occurs and its class is a sub-class (see *isa*)
      of one exception class given by the subsequent arguments
    * If there is no matched argument class, the exception is propagated further
```
          var ln;
          for (; try (ln = getln (), eof);) putln (ln);
```
  * In the previous example, `try (ln = getln (), eof)` can be considered as
    abbreviation of anonymous function call:
```
          fun {try {ln = getln (); return 1;} catch (eof) {return 0;} ()
```

# Earley parser
* Predefined class for language prototyping:
    * Fast.  Processing ~400K lines/sec of 67K lines of C program
      using 26MB memory on modern CPUs
    * Simple syntax directed translation
    * Parsing input can be described by ambiguous grammar:
        * Can produce compact representation of all possible parse trees
        * Can produce minimal cost parsing tree
    * Syntax recovery with minimal number of ignored tokens
      still producing a correct AST

---

# Earley parser -- tiny language example
````
expose yaep.*;
val grammar =
 "TERM ident=301, num=302, if=303, then=304, for=305, do=307, var=308;
  program = program stmt                     # list (0 1)
  stmt = ident '=' expr ';'                  # asgn (0 2)
       | if expr then stmt else stmt         # if (1 3 5)
       | for ident '=' expr expr do stmt     # for (1 3 4 6)
       | '{' program '}'                     # block (1)
       | var ident ';'                       # var (1)
       | error
  expr = expr '+' factor                     # plus (0 2)
  factor = factor '*' term                   # mult (0 2)
  term = ident                               # 0
       | '(' expr ')'                        # 1";
val p = parser ();       // create an Earley parser
p.set_grammar (grammar); // set grammar
fun syntax_error;        // forward decl of syntax error reporting func
val asbtract_tree = p.parse (token_vector, syntax_error);
````

---

# Implementation -- General Structure
![Dino Flow](Dino_Flow.png "Dino Flow")

* In usual mode, all program files are processed.
* In REPL mode, a statement goes all processing.
* All program Byte Code (Bcode) can be saved in a readable form,
  modified, and read for execution.
* Function level JIT is implemented with the aid of C compiler.
* The program can use object files created from a C code
  (through Foreign Function Interface).

---

# Implementation -- Byte Code
* Byte Code (Bcode) consists of
    * Declarations
        * vdecl (variable)
        * fdecl (functions, classes, threads)
    * Multi-operand instructions
        * Operations (1-5 ops, usually 3-ops)
        * Control flow insns (blocks, branches, calls etc)
* 2 Bcode representations:
    * one in memory (for execution)
    * readable representation (can be modified manually)

---

# Implementation -- Byte Code example
* Dino code
```
var i, n = 1000;
for (i = 0; i < n; i++);
```
* Readable BCode representation:
```
0 block fn="ex.d" ln=1 pos=1 next=730 vars_num=29 tvars_num=3 // ident=
...
372 vdecl fn="ex.d" ln=1 pos=5 ident=i ident_num=268 decl_scope=0 var_num=27
373 vdecl pos=8 ident=n ident_num=269 decl_scope=0 var_num=28
...
788 ldi fn="ex.d" ln=1 pos=12 op1=28 op2=1000 // 28 <- i1000
789 ldi ln=2 pos=10 next=791 op1=27 op2=0 // 27 <- i0
790 btltinc pos=15 next=792 op1=27 binc_inc=1 bcmp_op2=28 bcmp_res=29 pc=790
                                          // goto 790 if 29 <- (27 += i1) cmp 28
791 btlt pos=15 op1=27 bcmp_op2=28 bcmp_res=29 pc=790 // goto 790 if 29 <- 27 cmp 28
792 bend pos=17 block=0
```

---

# Implementation -- BC optimizations
* Optimizations
    * High-level dead code elimination
    * Jump optimization
    * Call tail optimization
    * Inlining
    * Pure function optimization
    * Byte code combining (this is just an illustration, the readable
      BCode representation has a bit different format -- see the previous slide)
```
      label: addi op1, op1, i1; lt res, op1, op2; bt res, label =>
      label: addi op1, op1, i1; blt res, op1, op2, label =>
      label: btltinc op1, op2, i2, res, label
```

---

# Implementation
* Fast optimizing interpreter
* Memory Handling and Garbage Collection:
    * Automatically extended heap
    * Simple escape analysis to transform heap allocations into stack ones
    * Combination of Mark and Sweep and fast Mark and Copy algorithm
      permitting to decrease program memory requirement



---

# Implementation -- Continuation
* JIT
    * Function Level for functions marked by hint (! jit)
```
      fun fact (n) !jit {n <=1 ? 1 : n * fact (n - 1);}
```
* JIT details:
    * Triggered by the first call
    * Portable implementation through C code generation
        * memory file system is used (can be persistent memory in future)
        * option --save-temps can be used for the C code inspecting
    * Usage of the same code as interpreter to simplify implementation
        * C code generator is less 100 lines on C
        * a script used to minimize the code (about 1/10 of C code
          interpreter definitions are used for generated code.)
    * Small function code generation takes about 50-70ms
      using GCC on modern Intel CPUs


---

# Implementation -- Type Inference
* Dino is dynamic type programming language
* Still many types of operations can be recognized during compilation time:
    * E.g. general Bcode **add** can be changed by **iadd** (integer variant)
      or **fadd** (floating point variant) which are executed
      without operand type checking
* Type recognition (inference) is very important for better
  object code generation, **especially for JIT**.
    * It can speed up code in many times


---

# Implementation -- Type Inference 2
* Major steps:
    1. Building **CFG** (control flow graph) of **all program**:
       basic blocks and CFG edges connecting them
    2. Calculating **available** results of Bcode insns -- a forward
       data-flow problem on CFG
    3. Using the availability, building **def-use chains** connecting
       operands and results of Bcode insns and variables
    4. Calculating types of Bcode insn operands and results -- another
       forward data flow problem on the built def-use graph
    5. Changing Bcode insns on specialized ones, e.g. **add** on **iadd**


---

# Implementation -- Type Inference 3
* Major complications:
    * Higher order functions
    * Closures
    * Threads
    * Possible use of variable (undefined) value before assigning a value to it
* Therefore we don't recognize all types theoretically possible to recognize
* Recognizing types of variable values even if the variable changes
  its type -- difference to type inference in static type languages


---

# Implementation -- Tools and libraries
* Dino is implemented with COCOM tools
    * SPRUT - compiler of IR object oriented description.  Used for
      implementation of semantic IR, Bcode and run-time data.
      In debugging mode, it permits to check all described constraints and relations
    * MSTA - faster superset of YACC with better error recovery
    * SHILKA - fast keyword recognizer generator
    * AMMUNITION - different packages (source position handling, error reporting,
      Earley parser etc)
* GMP - multi-precision integer library
* Oniguruma regexp library

---

# Implementation -- Profiling
* Typical performance tuning: Profiling: dino -p meteor.d
```
** Calls *** Time **** Name **************************************
  761087        0.43  --  search1: "meteor.d": 229
  561264        0.07  --  ctz: "meteor.d": 28
    1260        0.01  --  GoodPiece: "meteor.d": 37
     ...
                0.51  --  All Program
```
* Adding hints: !inline for ctz and !jit for search1

```
** Calls *** Time **** Name **************************************
  761087        0.15  --  search1: "meteor.d": 229
     ...
       0        0.00  --  ctz: "meteor.d": 28
     ...
                0.17  --  All Program
```

---

# Implementation -- C/C++ Interface
* Interface to C/C++


---

---

# Code Metrics
* sloccount output as of 2/18/2016
  * Dino + tools:
```
	Totals grouped by language (dominant language first):
	sh:          265452 (54.10%)
	ansic:       194472 (39.64%)
	yacc:         23297 (4.75%)
	cpp:           7403 (1.51%)
```
  * Dino directory only:
```
	Totals grouped by language (dominant language first):
	sh:          161561 (62.13%)
	ansic:        95124 (36.58%)
	yacc:          3365 (1.29%)
```

---

# Benchmarking -- Programs
* Some old computer language shootout benchmarks:
    * loop - empty loop body
    * hash - associative tables
    * fact, fib - factorial and fibonacci (recursive functions with and
      without tail recursion)
    * exceptions, methods, objects - exception processing, object method calls,
      and object instantiations
    * sieve, sort - Eratosthenes sieve and heapsort (array benchmarking)
    * statistics, random - statistical moments and random number generator
     (general arithmetic)
    * threads (producer-consumer threads)
    * startup - compilation and execution of empty program
    * compile -  very long code of assignments

---

# Benchmarking -- Languages and CPUs
* Benchmarking on x86-64 (i5-4670 - 3.4GHz Haswell), AARCH64 (X-gene),
  ARM (Exynos 5410 - 1.6GHz Cortex-A15), PPC64 (3.5GHz power7) and comparison with:
    * Other interpreters (Python-3.3.x, Ruby-2.0.x)
    * Different JITs (PyPy-2.2.x - trace JIT for Python, JavaScript-1.8.x
      - SpiderMonkey/TraceMonkey, Scala-2.10.x - JVM)
    * Byte code compiler and interpreter (OCAML-4.0.x)
* Dino compilation and execution time is a **base** in the comparison


---

# Benchmarking -- x86-64

|       |Loop   |Hash|Fact   |Fib    |Except|Method|Object|Sieve|Sort   |Stat.|Random |Thread|Start|Compile|
:-------|------:|---:|------:|------:|-----:|-----:|-----:|----:|------:|---: |------:|-----:|----:|------:|
Dino[^1]|1.0[^2]|1.0 |1.0[^3]|1.0[^3]|1.0   |1.0   |1.0   | 1.0 |1.0[^2]|1.0  |1.0[^4]|1.0   | 1.0 | 1.0   |
Dino    |23     |1.0 |19.5   | 69    |1.0   |1.0   |1.0   | 1.0 |1.4    |1.   |  3.5  |1.0   | 1.0 | 1.0   |
Python  |416    |2.2 |156    |483    |9.0   |7.8   |6.0   |54.6 |9.4    |3.6  |34.6   |190   |26.3 |  3.9  |
Ruby    |237    |2.8 |44.6   |148    |4.6   |1.9   |2.5   |  8.3|3.1    |4.6  |13.4   |194   |25.5 |  2.0  |
PyPy    |23.7   |0.5 | 0.6   |  68   |0.5   |0.3   |0.1   |  6.9|1.2    |1.5  |  1.3  |76.5  |22.1 |21.3   |
JS      |204    |1.1 |52.4   |167    |   -  |   -  |  -   |  7.8|0.5    |  -  |  0.7  |  -   |  0.8|  1.9  |
Scala   |10.7   |1.1 | 3.3   |114    |8.9   |1.4   |0.8   |  2.8|0.5    |7.1  |  1.3  |  -   |377  |  -[^5]|
Ocaml   | 2.3   |1.1 | 7.2   |  54   |0.3   |0.6   |1.0   |  2.2|1.1    |3.5  |  1.6  |  -   |  7.9|353    |

[^1]: Dino best result.
[^2]: Dino JIT hint was used.
[^3]: Dino pure func hint was used.
[^4]: Dino inline hint was used.
[^5]: Scala can not even handle 10 times smaller code.


---

# Benchmarking -- AARCH64

|       |Loop   |Hash   |Fact   |Fib    |Except|Method|Object|Sieve|Sort   |Stat.|Random |Thread|Start|Compile|
:-------|------:|------:|------:|------:|-----:|-----:|-----:|----:|------:|---: |------:|-----:|----:|------:|
Dino[^1]|1.0[^2]|1.0    |1.0[^3]|1.0[^3]| 1.0  | 1.0  |1.0   | 1.0 |1.0[^2]|1.0  |1.0[^4]| 1.0  | 1.0 |1.0    |
Dino    |17.5   |1.7    |13.1   | 265   | 1.0  | 1.0  |1.0   | 1.0 | 1.4   |1.0  | 2.2   | 1.0  | 1.0 |1.0    |
Python  |222    |0.5    | 68.8  |1116   |12.4  | 4.7  |2.8   |40.3 | 5.3   |2.5  |15.5   |149   |246  |2.6    |
Ruby    |170    |2.8    |32.3   |  542  |  6.0 | 1.7  |1.4   | 8.5 | 3.7   |3.8  |13.1   |118   |655  |1.6    |
JS      |282    |1.4[^6]|31.6   |  471  |   -  |  -   |  -   | 16.2| 2.5   |  -  | 6.5   | -    | 1.0 |0.5    |
Ocaml   |44.7   |1.2    |  5.0  |  166  |  0.3 | 0.5  |0.9   | 2.6 | 2.1   |4.0  | 3.5   | -    |82.7 |232    |

* Python v2.7.5 was used as Python3 is absent.
* PyPy and Scala are not implemented yet.

[^6]: Non-JIT JS was used as JIT failed.


---

# Benchmarking -- ARM

|       |Loop   |Hash   |Fact   |Fib    |Except|Method|Object|Sieve|Sort   |Stat.|Random |Thread|Start|Compile|
:-------|------:|------:|------:|------:|-----:|-----:|-----:|----:|------:|---: |------:|-----:|----:|------:|
Dino[^1]|1.0[^2]|1.0    |1.0[^3]|1.0[^3]| 1.0  | 1.0  |1.0   | 1.0 |1.0[^2]|1.0  |1.0[^4]| 1.0  | 1.0 |1.0    |
Dino    |4.2    |1.0    |18.1   | 490   | 1.0  | 1.0  |1.0   | 1.0 | 1.8   |1.0  | 2.5   | 1.0  | 1.0 |1.0    |
Python  |28.2   |0.6    |  4.4  |1600   |11.7  | 3.4  |5.1   |19.3 | 6.7   |2.2  |17.5   |157   |10.9 |2.7    |
Ruby    |31.5   |2.6    |71.2   |  651  |  6.3 | 1.3  |1.7   | 4.8 | 4.0   |3.3  |11.3   |174   |12.7 |1.7    |
PyPy    |103    |1.4    |151    |4158   |  9.9 |12.1  |7.9   |42.1 |11.7   |5.5  |30.4   |485   | 7.2 | [^7]  |
JS      |29.8   |1.4[^6]|32.2   |  634  |   -  |  -   |  -   | 4.1 | 0.4   |  -  | 0.4   | -    | 1.1 |0.7    |
Scala   | 7.4   |8.2    |  6.4  |2396   |  6.9 | 1.2  |1.0   | 5.8 | 0.7   |19   | 1.2   | -    |109  | [^7]  |
Ocaml   |13.8   |1.0    |  8.8  |  327  |  0.4 | 0.6  |1.0   | 2.5 | 2.2   |3.3  | 1.7   | -    | 3.0 | [^7]  |

[^7]: PyPy, Scala, and Ocaml failed to compile long code.


---

# Benchmarking - PPC64

|       |Loop   |Hash   |Fact   |Fib    |Except|Method|Object|Sieve|Sort   |Stat.|Random |Thread|Start|Compile|
:-------|------:|------:|------:|------:|-----:|-----:|-----:|----:|------:|---: |------:|-----:|----:|------:|
Dino[^1]|1.0[^2]|1.0    |1.0[^3]|1.0[^3]|1.0   |1.0   |1.0   |1.0  |1.0[^2]|1.0  |1.0[^4]|1.0   |1.0  |1.0    |
Dino    |       |1.0    |       |       |1.0   |1.0    |1.0  |1.0  |       |1.0  |       |1.0   |1.0  |1.0    |
Python  |25.3   |0.5    |65.9   |592    |12.9  |4.3   |6.1   |25.1 |4.3    |2.0  |6.9    |166   |62.5 |2.4    |
Ruby    |17.6   |1.7    |43.0   |390    |20.0  |1.6   |3.1   |7.6  |4.2    |5.2  |11.4   |62.8  |46.1 |1.5    |
JS      |27.0   |3.0    |53.8   |453    |   -  |   -  |  -   |14.2 |2.8    |  -  |4.6    |  -   |2.7  |0.7    |
Ocaml   |8.2    |1.1    |11.7   |165    |0.4   |0.8   |1.9   |3.7  |2.1    |4.8  |2.4    |  -   |27.7 |240    |

* PyPy is not implemented for PPC64.
* Scala was not available.

---

# Implementation - Conclusions
* A lot of research was done on Dino implementation
  (see [article about this](https://github.com/dino-lang/implemenation-article))
* It can be used to improve performance of popular dynamic language implementations:
    * to make them faster
    * to make them more portable
    * to require less resources (compilation time and memory)
    * to have very quick start up and big compiler speed

---

# Future directions of research
* Type annotation.  Two goals:
  * More cases for compile type checking which is in a direction of
    Dino language development (introduction of early undefined value
    recognition, the same number of actual and formal parameter
    numbers etc.)
  * Faster code generated by JIT
* Light-weight direct JIT
  * Using GCC for JIT is portable but too heavy for some system as CYGWIN
  * Direct JIT from bytecode to machine instructions is necessary
    * Goal is very fast code generation and simple fast optimizations
      to decrease memory traffic
    * With type inference and type annotation the direct JIT can
      achives 1/2-1/3 of speed optimized C code for majority of programs

---

# Dino availability
* Dino has been implemented for
  * Linux
  * Windows through CYGWIN
  * MacOSX
    * X code and GMP package is necessary to build Dino
* DINO and COCOM repository:
      <https://github.com/dino-lang/dino.git>
* License -- GPL 2 and LGPL 2:
    * See files COPYING and COPYING.LIB

---

# Dino Building
* See file INSTALL for details
* Configure in a build directory:
```
  <dino-path>/configure --srcidir=<dino-path> --prefix=<install-path>
```
* Configure in a debug mode: -O0 and full IR checking (it makes
  DINO several times slower):
```
  dino_debug=y <dino-path>/configure --srcidir=<dino-path> --prefix=<install-path>
  or cocom_debug=y ...
```
* Make:
```
  make
```
* Testing all COCOM and DINO:
```
  make check
```
* Testing only DINO (about 900 tests and benchmarking comparison with available implementations of other languages which can take a lot of time):
```
  cd DINO; make check
```

* Testig Dino is to run two shell scripts:
    * Tests are in file <build-directory>/DINO/dino.tst generated from DINO/dino.tst.in
    * Benchmarks are in file DINO/compare.tst

* Installing COCOM and DINO:
```
  make install
```
