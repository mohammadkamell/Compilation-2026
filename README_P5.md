# Dolphin Compiler — Phase 5
 
A compiler for the Dolphin language that produces LLVM IR, built with OCaml and Dune.
 
Phase 5 extends the language with:
 
- **Strings:** string literals (with escape sequences and literals spanning several lines),
  comparison by value, `length_of`, and the string functions of the standard library
- **Records:** top-level, mutually recursive record declarations, creation with `new`,
  field access with dot notation, `nil`, and comparison by reference
- **Arrays:** creation with `new T[n]`, bounds-checked indexing, `length_of`, and
  comparison by reference
- A new runtime and standard library (`runtime.c`, `stdlib.c`, `runtime.h`)
## Prerequisites
 
All tools are pre-installed in the provided Docker Dev Container:
 
- OCaml + Dune
- opam libraries: `yojson`, `ppx_yojson_conv_lib`, `printbox`, `printbox-text`, `cmdliner`, `menhir`
- `clang` (for linking the runtime and running compiled programs)
- `dolphin-serialize` (reference parser, used by rescue mode)
> **Open in Dev Container** (VS Code): press `Ctrl+Shift+P` -> *Dev Containers: Reopen in Container*
 
---
 
## Project Structure
 
```
.
├── src/
│   ├── frontend/     # AST, lexer (ocamllex), parser (Menhir), type-checker (Semant),
│   │                 # Env, Errors, pretty-printers
│   ├── ir/           # LLVM IR types (Ll), TypedAst
│   ├── backend/      # Code generation (Codegen), CFG builder
│   ├── util/         # Symbol table & source locations
│   └── runtime/      # C runtime (runtime.c), standard library (stdlib.c), runtime.h
├── bin/              # Main CLI entry point + Deserializer + Compiler pipeline
├── test/
│   ├── test_codegen.ml       # Codegen unit tests from phase 1 (hand-built typed ASTs)
│   ├── codegen5/             # Codegen unit tests for strings, records and arrays
│   ├── semantic/             # Unit tests for semantic analysis (Ast -> TypedAst)
│   ├── parser/               # Parser tests against the reference parser
│   └── tests/
│       ├── positive/              # Valid programs from phases 1-2, with expected output
│       ├── negative/              # Programs rejected by semantic analysis (phases 1-2)
│       ├── assignment-05-tests/   # Lexer and parser errors (phase 3)
│       ├── assignment-07-tests/   # Functions and comma expressions (phase 4)
│       ├── assignment-08-tests/   # Strings, records and arrays (phase 5)
│       └── examples/              # Example programs provided with the course (+ .out files)
├── run_tests.sh      # Runs our own test corpus
├── run_examples.sh   # Runs the example programs provided with the course
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
 
The integration entry points in `bin/compiler.ml`:
 
```ocaml
val compile_prog_from_ast : Ast.program -> Ll.prog option
val compile_prog_from_filename : string -> Ll.prog option
```
 
`compile_prog_from_filename` runs the whole pipeline on a `.dlp` file: lexer, parser,
semantic analysis and code generation. `compile_prog_from_ast` runs semantic analysis and
code generation on an AST that has already been built (used by `--from-ast` and `rescue`).
 
Both return `Some ll_prog` on success. On a lexical, syntax or semantic error they print the
error message(s) to stderr and return `None`, so the compiler exits with exit code 1.
 
---
 
## Running the Compiler
 
### Normal mode: from a `.dlp` source file (used for grading)
 
Uses our own lexer and parser.
 
```bash
dune exec dolphin -- compile --phase 5 <file.dlp>
```
 
### Rescue mode: with the reference parser
 
Uses the reference parser (`dolphin-serialize`) instead of our own lexer and parser, and
then runs our semantic analysis and code generation.
 
```bash
dune exec dolphin -- rescue --phase 5 <file.dlp>
```
 
### From a serialized AST
 
```bash
dolphin-serialize serialize --phase 5 --output prog.json prog.dlp
dune exec dolphin -- compile --from-ast --phase 5 prog.json
```
 
### Compiling and running the generated program
 
The compiler prints LLVM IR on stdout. Link it with the runtime and the standard library:
 
```bash
dune exec dolphin -- compile --phase 5 prog.dlp > prog.ll
clang prog.ll src/runtime/runtime.c src/runtime/stdlib.c -o prog
./prog; echo "exit code: $?"
```
 
### Exit codes
 
**Of the compiler:**
 
| Exit code | Meaning |
|-----------|---------|
| `0`   | Program compiled successfully (LLVM IR printed on stdout) |
| `1`   | Program rejected by the lexer, parser or semantic analysis (error printed on stderr) |
| other | Internal error / crash (e.g. `125` for an uncaught exception) |
 
**Of a compiled program:** the value returned by `main`, or `1` if a runtime error occurs
(nil access, array index out of bounds, negative array length or division by zero); the
runtime then prints an error message on stderr.
 
---
 
## Running Tests
 
### All tests
 
```bash
make tests                # with our own frontend
make tests MODE=rescue    # with the reference parser
```
 
This runs both test scripts below. `make` fails if either of them fails.
 
### Our own test corpus (`run_tests.sh`)
 
```bash
bash run_tests.sh               # with our own frontend
MODE=rescue bash run_tests.sh   # with the reference parser
```
 
- **Positive tests** are compiled, linked with the runtime, and executed with a
  5-second timeout (to detect infinite loops). Their output and exit code are compared
  with the expected values written in a comment at the top of each test file:
```
// Expected output: 0 1 2
// Expected exit code: 0
```
 
  The printed lines are joined into one space-separated line before the comparison.
  Possible results: `OK`, `WRONG` (output or exit code differs), `TIMEOUT`,
  `FAILED (compile)` or `FAILED (clang)`.
 
- **Negative tests** must be rejected by the lexer, parser or semantic analysis with
  **exit code 1**. Exit code `0` is reported as `UNEXPECTED OK`, and any other exit code
  as `CRASH`, so that a crash is never mistaken for a correct rejection.
In `assignment-08-tests`, positive tests are named `pos_*.dlp` and negative tests `neg_*.dlp`.
 
### Example programs (`run_examples.sh`)
 
```bash
bash run_examples.sh               # with our own frontend
MODE=rescue bash run_examples.sh   # with the reference parser
```
 
Runs the example programs provided with the course (`test/tests/examples`) and compares
their output with the corresponding `.out` file, which contains the expected standard
output followed by the exit code on the last line.
 
### Unit tests
 
```bash
make semanttest                              # semantic analysis
make test                                    # codegen (phase 1 programs)
dune exec ./test/codegen5/test_strings.exe   # codegen for strings (prints LLVM IR)
dune exec ./test/codegen5/test_records.exe   # codegen for records (prints LLVM IR)
dune exec ./test/codegen5/test_arrays.exe    # codegen for arrays (prints LLVM IR)
bash test/parser/run_parser_tests.sh         # parser vs. reference parser (phase 4)
```
 
---
 
## Test Corpus Overview
 
| Directory | Phase | Tests | What it covers |
|-----------|-------|-------|----------------|
| `positive/` | 1-2 | 33 | Arithmetic, comparisons, booleans, variables, scoping, short-circuiting, division, loops, `break`/`continue`, multiple declarations |
| `negative/` | 1-2 | 28 | Type errors, unknown names, invalid statements, `break`/`continue` outside loops, scoping errors |
| `assignment-05-tests/` | 3 | 10 | Lexer and parser errors |
| `assignment-07-tests/` | 4 | 12 | Functions, recursion, mutual recursion, void functions, comma expressions |
| `assignment-08-tests/` | 5 | 31 | Strings, records, arrays, runtime errors (see below) |
| `examples/` | 5 | 12 | Example programs provided with the course |
 
All tests from earlier phases are kept as regression tests.
 
### Phase 5 positive tests (`assignment-08-tests/pos_*.dlp`)
 
| File | What it covers |
|------|----------------|
| `pos_01_string_escapes.dlp` | Escape sequences (`\t`, `\n`, `\"`, `\\`) are unescaped by the lexer |
| `pos_02_multiline_string.dlp` | A string literal spanning two lines equals the same string written with `\n` |
| `pos_03_string_comparison.dlp` | Strings compared by value and lexicographically, also when built at runtime |
| `pos_04_record_field_order.dlp` | Fields initialized in a different order than declared |
| `pos_05_record_eval_order.dlp` | Field initializers are evaluated in source order |
| `pos_06_mutual_records.dlp` | Mutually recursive records, including a cycle built at runtime |
| `pos_07_record_reference_and_nil.dlp` | Records are compared by reference; aliasing; comparison with `nil` |
| `pos_08_empty_record.dlp` | A record without fields is non-nil and distinct from another one |
| `pos_09_arrays_2d_and_defaults.dlp` | Arrays of arrays; default values for `int`, `string` and `bool` |
| `pos_10_array_of_records.dlp` | `new Cell[2]` vs. `new Cell { ... }`; long lval chains; default `nil` |
| `pos_11_runtime_nil_access.dlp` | Field access on `nil` stops the program with exit code 1 |
| `pos_12_runtime_index_too_large.dlp` | Index equal to the length is out of bounds (exit code 1) |
| `pos_13_runtime_negative_index.dlp` | Negative index is out of bounds (exit code 1) |
| `pos_14_runtime_negative_length.dlp` | Array with negative length (exit code 1) |
| `pos_15_runtime_length_of_nil.dlp` | `length_of` on a `nil` array (exit code 1) |
 
### Phase 5 negative tests (`assignment-08-tests/neg_*.dlp`)
 
| File | Error tested |
|------|--------------|
| `neg_01_new_reserved_record.dlp` | `new` on a reserved record (`stream`) |
| `neg_02_reserved_record_name.dlp` | User-defined record with a reserved name |
| `neg_03_duplicate_record.dlp` | Two records with the same name |
| `neg_04_duplicate_field.dlp` | Two fields with the same name in a record |
| `neg_05_missing_field_init.dlp` | A field is not initialized at record creation |
| `neg_06_unknown_field_access.dlp` | Access to a field the record does not have |
| `neg_07_field_init_type_mismatch.dlp` | Field initializer of the wrong type |
| `neg_08_record_arithmetic.dlp` | `+` on records |
| `neg_09_compare_different_records.dlp` | `==` on records of different types with identical fields |
| `neg_10_index_not_int.dlp` | Non-integer array index |
| `neg_11_array_element_type_mismatch.dlp` | Storing a value of the wrong type in an array |
| `neg_12_length_of_int.dlp` | `length_of` on an integer |
| `neg_13_unknown_record_type.dlp` | A record type that is never declared |
| `neg_14_string_arithmetic.dlp` | `-` on strings |
| `neg_15_unterminated_string.dlp` | String literal that is never closed (lexer error) |
| `neg_16_record_missing_semicolon.dlp` | Missing `;` between record fields (parser error) |
 
---
 
## Submission
 
Create the ZIP (replace `XY` with your group number):
 
```bash
zip -r groupXY.zip \
  src/ bin/ test/ \
  dune dune-project Makefile README.md run_tests.sh run_examples.sh \
  -x '*/_build/*' '*/.git/*'
```
 
Run the pre-submission checker:
 
```bash
bash presub.sh 5 groupXY.zip
```
 
All 3 checks must pass before submitting.
 