# Dolphin Compiler — Phase 3

A compiler for the Dolphin language that produces LLVM IR, built with OCaml and Dune.

Phase 3 extends the language from Phase 1 and Phase 2 with:

- Task 1: lexer with `ocamllex`
- Task 2: conflict-free parser with Menhir 
- Task 3: semantic errors now include locations 
- Task 4: `compile_prog_from_filename` (lexer → parser → semantic analysis → codegen) and tests ported to `.dlp`

## Prerequisites

All tools are pre-installed in the provided Docker Dev Container:

- OCaml + Dune
- opam libraries: `yojson`, `ppx_yojson_conv_lib`, `printbox`, `printbox-text`, `cmdliner`
- `clang` (for linking the runtime and running compiled programs)
- `dolphin-serialize` (reference parser, used by rescue mode)

> **Open in Dev Container** (VS Code): press `Ctrl+Shift+P` -> *Dev Containers: Reopen in Container*

---

## Project Structure

```
.
├── src/
│   ├── frontend/     # AST, type-checker (Semant), Env, Errors, pretty-printers
│   ├── ir/           # LLVM IR types (Ll), TypedAst
│   ├── backend/      # Code generation (Codegen), CFG builder
│   ├── util/         # Symbol table & source locations
│   └── runtime/      # C runtime (print_integer, read_integer, div-by-zero handler)
├── bin/              # Main CLI entry point + Deserializer + Compiler pipeline
├── test/
│   ├── test_codegen.ml       # Manual codegen unit tests (bypasses parser and semant)
│   ├── semantic/
│   │   └── test_semant.ml    # Unit tests for semantic analysis (Ast -> TypedAst)
│   └── tests/
│       ├── positive/         # Valid Dolphin programs with expected output
│       └── negative/         # Invalid programs (must be rejected by semant)
│       └── assignment-05-tests/    # Negative tests for lexer and parser errors 
├── run_tests.sh      # End-to-end test runner for the test corpus
├── dune
├── dune-project
└── Makefile
```

---

## Building

```bash
dune build
```

Clean build (recommended before submission):

```bash
dune clean && dune build
```

The compiler executable is exposed under the public name `dolphin`
(see `bin/dune` and the `dolphin` package in `dune-project`), so it can be run with
`dune exec dolphin -- ...`.

---

## Compiler Pipeline

```
Dolphin source (.dlp)
    |
    v
Lexer.token                  <- lexical analysis (ocamllex)
    |
    v
Parser.program               <- parsing (Menhir)
    |
    v
Ast.program
    |
    v
Semant.typecheck_prog        <- semantic analysis / type checking
   |
   v
TypedAst.program
   |
   v
Codegen.codegen_prog         <- LLVM IR code generation
  |
  v
Ll.prog  (printed as LLVM IR text)
```

The integration entry point in `bin/compiler.ml`:

```ocaml
val compile_prog_from_ast : Ast.program -> Ll.prog option 
val compile_prog_from_filename : string -> Ll.prog option
```

`compile_prog_from_ast` runs semantic analysis and codegen on an already built AST (used by `--rescue` mode). 

`compile_prog_from_filename` runs the whole pipeline on a `.dlp` file: lexer, parser, semantic analysis and codegen.  

Both return `Some ll_prog` on success. On a lexical, syntax or semantic error they print the error message(s) to stderr and return `None`, so the compiler exits with exit code 1.

---

## Running the Compiler

### Normal mode: directly from a `.dlp` source file

Uses our own lexer and parser (lexer, parser, semantic analysis, codegen).

```bash
dune exec dolphin -- compile --phase 3 <file.dlp>
```

### Rescue mode: from a serialized AST

Skips the frontend and starts from a JSON AST (useful for testing the later phases).

```bash
dune exec dolphin -- compile --rescue --phase 3 <ast.json>
```

A serialized AST can be produced from a Dolphin source file with the reference serializer:

```bash
dolphin-serialize serialize --phase 3 --output prog.json prog.dlp
```

### Compiling and running the generated program

The compiler prints LLVM IR on stdout. Link it with the C runtime using clang:

```bash
dune exec dolphin -- compile --phase 3 prog.dlp > prog.ll
clang prog.ll src/runtime/runtime.c -o prog
./prog; echo "exit code: $?"
```

### Exit codes

| Exit code | Meaning |
|-----------|---------|
| `0`   | Program compiled successfully (LLVM IR printed on stdout) |
| `1`   | Program rejected by the lexer, parser or semantic analysis (error printed on stderr) |
| other | Internal error / crash (e.g. `125` for an uncaught exception) |

---

## Running Tests

### End-to-end test corpus (`run_tests.sh`)

```bash
bash run_tests.sh
```

The script builds the project and then runs every program in `test/tests/`:

- **Positive tests** are compiled, linked with the C runtime, and executed with a
  5-second timeout (to detect infinite loops). Their output and exit code are compared
  with the expected values written in a comment at the top of each test file:

  ```
  // Expected output: 0 1 2
  // Expected exit code: 0
  ```

  Possible results: `OK`, `WRONG` (output or exit code differs), `TIMEOUT`,
  `FAILED (compile)` or `FAILED (clang)`.

- **Negative tests** must be rejected by semantic analysis with **exit code 1**.
  Exit code `0` is reported as `UNEXPECTED OK`, and any other exit code as `CRASH`,
  so that a crash is never mistaken for a correct rejection.

### Semantic analysis unit tests

`test/semantic/test_semant.ml` builds ASTs directly in OCaml, runs
`Semant.typecheck_prog`, and compares the result with the expected typed AST or the
expected error.

```bash
dune exec ./test/semantic/test_semant.exe
```

### Codegen unit tests (`make test`)

Runs `test/test_codegen.ml`, which hand-builds a `TypedAst.program` directly in OCaml
(bypassing the parser and type-checker), generates LLVM IR, compiles it with the C runtime,
and executes the result. The test case to run is selected by the `current` binding in
`test/test_codegen.ml`.

```bash
make test
```

---

## Test Corpus Overview

### Positive tests (`test/tests/positive/`)

Phase 1 (kept as regression tests):

| File | What it covers |
|------|----------------|
| `01_return.dlp` | Minimal program: `return 0;` |
| `02_arithmetic.dlp` | `+`, `-`, `*`, `/`, `%` |
| `03_comparisons.dlp` | `<`, `<=`, `>`, `>=` |
| `04_booleans.dlp` | `&&`, `\|\|`, `==`, `!=` |
| `05_variables.dlp` | Variable declarations and references |
| `06_assignment.dlp` | Variable assignment |
| `07_if_else.dlp` | `if`/`else` branches |
| `08_scoping.dlp` | Block scope |
| `09_shadowing.dlp` | Variable shadowing across nested scopes |
| `09_library_calls.dlp` | `print_integer` |
| `10_short_circuit.dlp` | `&&` and `\|\|` short-circuit evaluation |
| `11_all_binary_ops.dlp` | All binary operators combined |
| `12_unary_ops.dlp` | Unary `-` and `!` |
| `13_left_to_right.dlp` | Left-to-right evaluation of binary operands (side effects in one expression) |
| `14_division.dlp` | Division with div-by-zero guard |
| `15_nested_blocks.dlp` | Nested compound statements |
| `16_nested_assignment.dlp` | Chained re-assignments |

Phase 2:

| File | What it covers |
|------|----------------|
| `17_while_basic.dlp` | Basic `while` loop |
| `18_while_zero_iter.dlp` | `while` whose condition is false from the start |
| `19_for_full.dlp` | `for` with init, condition and update |
| `20_for_empty_parts.dlp` | `for(;;)` terminated by `break` |
| `21_for_init_expr.dlp` | `for` with an expression as init (variable lives on after the loop) |
| `22_continue_while.dlp` | `continue` in `while` jumps to the condition |
| `23_continue_for.dlp` | `continue` in `for` still runs the update |
| `24_nested_break.dlp` | `break` only exits the innermost loop |
| `25_nested_continue.dlp` | `continue` in an inner `for` nested in a `while` |
| `26_for_shadowing.dlp` | `for` variable shadows an outer variable; outer value restored after the loop |
| `27_return_in_loop.dlp` | `return` from inside an infinite loop |
| `28_multidecl.dlp` | Multiple declarations; later ones use earlier ones |
| `29_multidecl_shadow.dlp` | Initializer sees the outer binding before shadowing |
| `30_decl_in_loop_body.dlp` | Declaration inside a loop body, re-initialized every iteration |
| `31_while_short_circuit.dlp` | Short-circuit `&&` in a loop condition prevents division by zero |
| `32_break_dead_code.dlp` | Dead code after `break` is never executed |

### Negative tests (`test/tests/negative/`)

Phase 1 (kept as regression tests):

| File | Semantic error tested |
|------|-----------------------|
| `01_arithmetic_type_error.dlp` | Non-integer arithmetic operand |
| `02_comparison_type_error.dlp` | Non-integer comparison operand |
| `03_boolean_type_error.dlp` | Non-boolean `&&`/`\|\|` operand |
| `04_undefined_variable.dlp` | Use of undeclared variable |
| `05_assignment_type_error.dlp` | Assigning `bool` to `int` variable |
| `06_unknown_function.dlp` | Calling an unknown function |
| `07_wrong_argument_count.dlp` | Wrong number of arguments |
| `08_wrong_argument_type.dlp` | Wrong argument type |
| `09_declaration_type_error.dlp` | Initializer type mismatch with explicit annotation |
| `10_if_condition_not_bool.dlp` | Non-boolean `if` condition |
| `11_out_of_scope_variable.dlp` | Variable used outside its block |
| `12_invalid_expression_statement.dlp` | Expression statement that is not a call or assignment |
| `13_bad_return_type.dlp` | `return true;` (must return int) |
| `14_missing_return.dlp` | Program with no final return statement |
| `15_function_masked_by_variable.dlp` | Local variable shadows a function name |
| `16_equality_type_mismatch.dlp` | `==` on operands of different types |
| `17_void_variable.dlp` | Variable initialised with a void-returning call |

Phase 2:

| File | Semantic error tested |
|------|-----------------------|
| `18_break_outside_loop.dlp` | `break` outside any loop |
| `19_continue_outside_loop.dlp` | `continue` outside any loop |
| `20_break_in_if_outside_loop.dlp` | `break` inside an `if` that is not in a loop |
| `21_break_after_loop.dlp` | `break` after a loop (loop context must not leak) |
| `22_while_cond_not_bool.dlp` | Non-boolean `while` condition |
| `23_for_cond_not_bool.dlp` | Non-boolean `for` condition |
| `24_for_var_after_loop.dlp` | `for` variable used after the loop |
| `25_multidecl_forward_ref.dlp` | Declaration uses a variable declared later in the same block |
| `26_multidecl_type_error.dlp` | Type mismatch inside a declaration block |
| `27_multidecl_void.dlp` | Void call as initializer inside a declaration block |
| `28_for_update_type_error.dlp` | Ill-typed `for` update expression |

Phase 3 (lexer and parser errors):

| File | Error tested |
|------|--------------|
| `29_illegal_symbol.dlp` | Unexpected symbol (parser error) |
| `30_missing_ending_quation_mark.dlp` | Missing closing quotation mark (lexer error) |
| `31_missing_semicolon.dlp` | Missing `;` after a statement (parser error) |
| `32_missing_ending_parenthesis.dlp` | Missing closing `)` (parser error) |
| `33_missing_left_operand.dlp` | Binary operator without left operand (parser error) |
| `34_using_keyword_as_variable_name.dlp` | Keyword used as a variable name (parser error) |
| `35_missing_variable_name.dlp` | Declaration without a variable name (parser error) |
| `36_missing_if_before_else.dlp` | `else` without a preceding `if` (parser error) |
| `37_missing_parenthesis_while_loop.dlp` | Missing parenthesis in a `while` condition (parser error) |
| `38_comment_with_end.dlp` | Block comment never closed (lexer error) |

---

### Expected output in test files (Phase 2 update of `run_tests.sh`)
 
In Phase 1, `run_tests.sh` only checked whether the compiler succeeded or failed: positive tests were never executed, and a crash was counted as a correct rejection of a negative test.
For Phase 2 the script was updated so that positive tests are checked against an expected result written directly in each `.dlp` file:
 
```
// Expected output: 0 1 2
// Expected exit code: 0
```
 
For every positive test, `run_tests.sh` now:
 
1. Extracts the expected output from the `// Expected output...:` line (any text between
   `output` and the colon is allowed, e.g. `// Expected output (printed by print_integer):`),
   and the expected exit code from the `// Expected exit code:` line (defaults to `0`).
2. Compiles the program with the compiler and links the generated LLVM IR with the C runtime.
3. Runs the program with a 5-second timeout, with stdin redirected from `/dev/null`.
4. Joins the printed lines into one space-separated line (each `print_integer` prints one
   line) and compares it, together with the exit code, to the expected values.
If the output or exit code differs, the test is reported as `WRONG` together with the
expected and actual values, which makes it easy to see what went wrong. Negative tests are now only counted as passed when the compiler exits with exactly exit code `1`.

## Submission

Create the ZIP (replace `XY` with your group number):

```bash
zip -r groupXY.zip \
  src/ bin/ test/ \
  dune dune-project Makefile README.md run_tests.sh \
  -x '*/_build/*' '*/.git/*'
```

Run the pre-submission checker:

```bash
bash presub.sh 2 groupXY.zip
```

All 3 checks must pass before submitting.