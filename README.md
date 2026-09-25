# Dolphin Compiler — Phase 1

A compiler for the Dolphin language that produces LLVM IR, built with OCaml and Dune.

## Prerequisites

All tools are pre-installed in the provided Docker Dev Container:

- OCaml + Dune
- opam libraries: `yojson`, `ppx_yojson_conv_lib`, `printbox`, `printbox-text`, `cmdliner`
- `clang` (for linking the runtime and running compiled programs)

> **Open in Dev Container** (VS Code): press `Ctrl+Shift+P` -> *Dev Containers: Reopen in Container*

---

## Project Structure

```
.
├── src/
│   ├── frontend/     # AST, type-checker (Semant), Env, Errors, pretty-printers
│   ├── ir/           # LLVM IR types (Ll), TypedAst
│   ├── backend/      # Code generation (Codegen), CFG builder
│   ├── util/         # Symbol table
│   └── runtime/      # C runtime (print_integer, read_integer, div-by-zero handler)
├── bin/              # Main CLI entry point + Deserializer + Compiler pipeline
├── test/
│   ├── test_codegen.ml       # Manual codegen unit tests (bypasses parser)
│   └── tests/
│       ├── positive/         # Valid Dolphin programs (should compile)
│       └── negative/         # Invalid programs (rejected with a semantic error)
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

---

## Compiler Pipeline

```
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
```

Returns `Some ll_prog` on success, `None` on semantic error (errors printed to stderr).

---

## Running the Compiler

### Rescue mode — from a `.dlp` source file

Useful for manual and test-corpus testing. Uses the parser built into the container.

```bash
./_build/default/bin/main.exe rescue --phase 1 <file.dlp>
```

Example:

```bash
./_build/default/bin/main.exe rescue --phase 1 test/tests/positive/01_return.dlp
```

### Normal mode — from a serialized AST JSON file

This is the mode used for grading.

```bash
./_build/default/bin/main.exe compile --from-ast --phase 1 <ast.json>
```

---

## Running Tests

### Codegen unit tests (`make test`)

Runs `test/test_codegen.ml`, which hand-builds a `TypedAst.program` directly in OCaml
(bypassing the parser and type-checker), generates LLVM IR, compiles it with the C runtime,
and executes the result.

```bash
make test
```

To switch which test case runs, edit `test/test_codegen.ml` line 199:

```ocaml
let current = test_and_short_circuit   (* change to any test_* name *)
```

Available test cases: `test_neg_mul`, `test_call_print`, `test_read_mul`, `test_vardecl`,
`test_lval`, `test_assign`, `test_shadow`, `test_cmp`, `test_if`, `test_if_return`,
`test_div`, `test_and_short_circuit`.

---

### Full test corpus (rescue mode)

```bash
echo "=== POSITIVE TESTS ==="
for f in test/tests/positive/*.dlp; do
    result=$(./_build/default/bin/main.exe rescue --phase 1 "$f" 2>/dev/null && echo "OK" || echo "FAILED")
    printf "%-45s %s\n" "$f" "$result"
done

echo ""
echo "=== NEGATIVE TESTS ==="
for f in test/tests/negative/*.dlp; do
    result=$(./_build/default/bin/main.exe rescue --phase 1 "$f" 2>/dev/null && echo "UNEXPECTED_OK" || echo "OK (rejected)")
    printf "%-45s %s\n" "$f" "$result"
done
```

Expected: all positive -> `OK`, all negative -> `OK (rejected)`.

---

## Test Corpus Overview

### Positive tests (`test/tests/positive/`)

| File | What it covers |
|------|----------------|
| `01_return.dlp` | Minimal program: `return 0;` |
| `02_arithmetic.dlp` | `+`, `-`, `*`, `/`, `%` |
| `03_comparisons.dlp` | `<`, `<=`, `>`, `>=` |
| `04_booleans.dlp` | `&&`, `||`, `==`, `!=` |
| `05_variables.dlp` | Variable declarations and references |
| `06_assignment.dlp` | Variable assignment |
| `07_if_else.dlp` | `if`/`else` branches |
| `08_scoping.dlp` | Block scope |
| `09_shadowing.dlp` | Variable shadowing across nested scopes |
| `09_library_calls.dlp` | `print_integer`, `read_integer` |
| `10_short_circuit.dlp` | `&&` and `||` short-circuit evaluation |
| `11_all_binary_ops.dlp` | All binary operators combined |
| `12_unary_ops.dlp` | Unary `-` and `!` |
| `13_left_to_right.dlp` | Left-to-right evaluation order |
| `14_division.dlp` | Division with div-by-zero guard |
| `15_nested_blocks.dlp` | Nested compound statements |
| `16_nested_assignment.dlp` | Chained re-assignments |

### Negative tests (`test/tests/negative/`)

| File | Semantic error tested |
|------|-----------------------|
| `01_arithmetic_type_error.dlp` | Non-integer arithmetic operand |
| `02_comparison_type_error.dlp` | Non-integer comparison operand |
| `03_boolean_type_error.dlp` | Non-boolean `&&`/`||` operand |
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

---

## Submission

Create the ZIP (replace `XY` with your group number):

```bash
zip -r groupXY.zip \
  src/ bin/ test/ \
  dune dune-project Makefile README.md \
  --exclude '*/_build/*' --exclude '*/.git/*'
```

Run the pre-submission checker:

```bash
./presub.sh 1 groupXY.zip
```

All 3 checks must pass before submitting.
