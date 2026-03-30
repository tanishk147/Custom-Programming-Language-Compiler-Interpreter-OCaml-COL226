# Custom Programming Language — Compiler & Interpreter (OCaml · COL226)

A custom domain-specific language (DSL) focused on **matrix and vector mathematics**, implemented entirely in OCaml as part of the COL226 (Programming Languages) course at IIT Delhi. The project includes a full compiler/interpreter pipeline: lexer → parser → AST → type checker → interpreter.

---

## Table of Contents

- [Overview](#overview)
- [Language Features](#language-features)
  - [Types](#types)
  - [Operators](#operators)
  - [Matrix & Vector Operations](#matrix--vector-operations)
  - [Control Flow](#control-flow)
  - [I/O](#io)
  - [Comments](#comments)
- [Syntax Reference](#syntax-reference)
  - [Variable Declaration & Assignment](#variable-declaration--assignment)
  - [Control Structures](#control-structures)
  - [Matrix File Format](#matrix-file-format)
- [Project Structure](#project-structure)
- [Building](#building)
- [Running](#running)
- [Demo Programs](#demo-programs)
- [Architecture](#architecture)

---

## Overview

The language is designed for numerical linear-algebra computations. It supports:

- First-class matrix and vector types with dimension tracking at the type-check stage
- Rich set of built-in matrix operations (multiply, transpose, inverse, determinant, dot product)
- Imperative control flow (if/else, while, for)
- Reading matrices from text files at runtime

---

## Language Features

### Types

| Type keyword         | Description                          |
|----------------------|--------------------------------------|
| `int`                | Integer                              |
| `float`              | Floating-point number                |
| `bool`               | Boolean (`true` / `false`)           |
| `vector[n]`          | Fixed-size vector of *n* floats      |
| `matrix[m][n]`       | Fixed-size *m × n* matrix of floats  |

### Operators

| Category       | Operators                                 |
|----------------|-------------------------------------------|
| Arithmetic     | `+`  `-`  `*`  `/`  `%`                  |
| Comparison     | `=`  `<>`  `<`  `>`  `<=`  `>=`          |
| Logical        | `and`  `or`  `not`                        |
| Assignment     | `:=`                                      |
| Unary          | `-` (negation),  `sqrt`                   |

### Matrix & Vector Operations

| Syntax / Keyword          | Description                                       |
|---------------------------|---------------------------------------------------|
| `A'`                      | Transpose of matrix or vector `A`                 |
| `A * B`                   | Matrix multiplication (when both operands are matrices with compatible dimensions) |
| `A * v`                   | Matrix–vector multiplication                      |
| `u * v`                   | Dot product of two vectors of equal size          |
| `u . v`                   | Explicit dot product                              |
| `determinant A`           | Determinant of square matrix `A` (returns `float`)|
| `inv(A)`                  | Inverse of square matrix `A`                      |
| `sqrt expr`               | Square root (returns `float`)                     |
| `A[i]`                    | Row *i* of matrix `A`, or element *i* of vector   |
| `A[i][j]`                 | Element at row *i*, column *j* of matrix `A`      |
| `read_matrix("file", r, c)` | Read an *r × c* matrix from a text file         |

### Control Flow

```
if <bool_expr> then <command> else <command>
if <bool_expr> then <command>

while <bool_expr> do <command>

for <var> := <int_expr> to <int_expr> do <command>
```

### I/O

```
Print(expr);          // print a value to stdout
Input();              // read from stdin (no prompt)
Input(expr);          // read with a prompt expression
```

### Comments

```
// single-line comment

/* multi-line
   comment */
```

---

## Syntax Reference

### Variable Declaration & Assignment

```dsl
// Declaration without initialization
int x;
float y;
bool flag;
vector[3] v;
matrix[2][3] M;

// Declaration with initialization
int x := 42;
float pi := 3.14159;
bool done := false;
vector[3] v := [1.0, 2.0, 3.0];
matrix[2][2] A := [[1.0, 0.0], [0.0, 1.0]];

// Assignment to an already-declared variable
x := x + 1;
v[0] := 5.0;
A[1][1] := -1.0;
```

### Control Structures

```dsl
// If-then-else
if (x > 0) then {
    Print(x);
} else {
    Print(-x);
}

// While loop
while (norm > 0.001) do {
    norm := norm * 0.5;
}

// For loop (inclusive bounds, integer step of 1)
for i := 0 to 9 do {
    Print(i);
}
```

### Matrix File Format

Matrix data files are plain text with the following format:

```
<rows> <cols>
[[r0c0,r0c1,...],[r1c0,r1c1,...],...]
```

Example (`2x2` identity matrix):

```
2 2
[[1.0,0.0],[0.0,1.0]]
```

---

## Project Structure

```
.
├── ast.ml              # AST type definitions and pretty-printer
├── type_checker.ml     # Static type checker and symbol table
├── interpreter.ml      # Tree-walk interpreter / runtime evaluator
├── lexer.mll           # ocamllex lexer specification
├── parser.mly          # ocamlyacc parser grammar
├── main.ml             # Entry point (lexes, parses, type-checks, executes)
├── Makefile            # Build rules
├── input.txt           # Default input file for `make test`
└── Demo_input_files/   # Sample programs and their matrix data files
    ├── t1.dsl          # Least-squares regression (6×6)
    ├── t2.dsl          # Matrix arithmetic and linear solve (4×5, 5×4)
    ├── t3.dsl          # Vector summation with for-loop
    ├── t4.dsl          # Iterative matrix scaling with while-loop
    ├── t5.dsl / t6.dsl # Matrix multiplication (3×2 · 2×3)
    ├── t7.dsl          # Add a vector to each column of a matrix
    ├── t8.dsl          # Matrix inverse and verification (A · A⁻¹ = I)
    └── *.txt           # Corresponding matrix data files
```

---

## Building

**Prerequisites:** OCaml toolchain (`ocamlc`, `ocamllex`, `ocamlyacc`)

```bash
# Build the interpreter binary
make

# Remove all generated and compiled files
make clean
```

The build produces an executable named `interpreter`.

---

## Running

```bash
# Run a DSL source file
./interpreter path/to/program.dsl

# Run using the default input.txt
make test

# Read from stdin
./interpreter
```

**Example:**

```bash
./interpreter Demo_input_files/t8.dsl
```

The interpreter prints a debug token stream, the parsed AST, and then executes the program, producing the program output on stdout.

---

## Demo Programs

| File   | Description |
|--------|-------------|
| `t1.dsl` | Least-squares regression: reads a 6×6 matrix, computes `(AᵀA)⁻¹Aᵀb` |
| `t2.dsl` | Matrix arithmetic: adds two 4×5 matrices, multiplies result with a 5×4 matrix, solves `Ex = u` |
| `t3.dsl` | Reads a 4-element column vector and computes `2.5 × Σvᵢ` with a for-loop |
| `t4.dsl` | Computes the Frobenius norm and halves each element in a while-loop until the norm falls below a threshold |
| `t5.dsl` | Multiplies 3×2 and 2×3 matrices in both orders |
| `t7.dsl` | Adds a 3-element vector to every column of a 3×3 matrix |
| `t8.dsl` | Reads a 2×2 matrix, prints its determinant, inverse, and verifies `A · A⁻¹ = I` |

---

## Architecture

```
Source file (.dsl)
      │
      ▼
  Lexer (lexer.mll)
  • Tokenises the input using ocamllex
  • Skips whitespace and comments
      │
      ▼
  Parser (parser.mly)
  • LALR(1) grammar via ocamlyacc
  • Calls type-checker helpers inline during parsing
  • Produces an Ast.command tree
      │
      ▼
  Type Checker (type_checker.ml)
  • Symbol table (Hashtbl) tracking variable types
  • Resolves operator overloading (e.g. * dispatches to MatrixMult vs DotProduct)
  • Raises TypeError on mismatches
      │
      ▼
  AST (ast.ml)
  • Algebraic data types for expressions and commands
  • Pretty-printer for debug output
      │
      ▼
  Interpreter (interpreter.ml)
  • Tree-walk evaluator
  • Runtime environment (Hashtbl of string → value)
  • Raises RuntimeError on division by zero, bad indices, etc.
```
