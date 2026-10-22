(* Direct Ast.program -> Semant.typecheck_prog -> TypedAst.program tests.
   No parser, deserializer, backend, LLVM execution, or test framework.

   Phase 3: source ASTs carry locations (dummy locations here), and semantic
   analysis collects all errors and raises Semant.TypeError with one message
   per error, in source order. *)
module A = Ast
module T = TypedAst

exception Test_failure of string

let fail message = raise (Test_failure message)
let equal label expected actual =
  if expected <> actual then fail (label ^ ": unexpected typed AST")

(* ------------------------------------------------------------------------
   Source-AST builders (all nodes get a dummy location)
   ------------------------------------------------------------------------ *)
let loc = Location.dummy_loc

let id name = A.Ident {name; loc}
let int n = A.Integer {int = Int64.of_int n; loc}
let bool value = A.Boolean {bool = value; loc}
let var name = A.Lval (A.Var (id name))
let bin left op right = A.BinOp {left; op; right; loc}
let unary op operand = A.UnOp {op; operand; loc}
let call name args = A.Call {fname = id name; args; loc}
let assign name rhs = A.Assignment {lvl = A.Var (id name); rhs; loc}
let decl ?tp name body = A.Declaration {name = id name; tp; body; loc}
let dblock ds = A.DeclBlock {declarations = ds; loc}
let decls ds = A.VarDeclStm (dblock ds)
let expr value = A.ExprStm {expr = Some value; loc}
let empty_stm = A.ExprStm {expr = None; loc}
let block stms = A.CompoundStm {stms; loc}
let ret value = A.ReturnStm {ret = value; loc}
let if_ ?els cond thbr = A.IfThenElseStm {cond; thbr; elbro = els; loc}
let while_ cond body = A.WhileStm {cond; body; loc}
let for_ ?init ?cond ?update body = A.ForStm {init; cond; update; body; loc}
let break_ = A.BreakStm {loc}
let continue_ = A.ContinueStm {loc}
let program stms = stms @ [ret (int 0)]

(* Types and operators (named op_* because land/lor are OCaml keywords). *)
let ty_int = A.Int {loc}
let ty_bool = A.Bool {loc}
let op_plus = A.Plus {loc}   let op_minus = A.Minus {loc}  let op_mul = A.Mul {loc}
let op_div = A.Div {loc}     let op_rem = A.Rem {loc}
let op_lt = A.Lt {loc}       let op_le = A.Le {loc}        let op_gt = A.Gt {loc}
let op_ge = A.Ge {loc}       let op_eq = A.Eq {loc}        let op_neq = A.NEq {loc}
let op_land = A.Land {loc}   let op_lor = A.Lor {loc}
let op_neg = A.Neg {loc}     let op_lnot = A.Lnot {loc}

(* ------------------------------------------------------------------------
   Expected typed-AST builders. These do not call semantic analysis.
   ------------------------------------------------------------------------ *)
let tid name = T.Ident {sym = Symbol.symbol name}
let tint n = T.Integer {int = Int64.of_int n}
let tbool value = T.Boolean {bool = value}
let tvar name tp = T.Lval (T.Var {ident = tid name; tp})
let tbin left op right tp = T.BinOp {left; op; right; tp}
let tunary op operand tp = T.UnOp {op; operand; tp}
let tcall name args tp = T.Call {fname = tid name; args; tp}
let tassign name rhs tp =
  T.Assignment {lvl = T.Var {ident = tid name; tp}; rhs; tp}
let tdecl name tp body = T.Declaration {name = tid name; tp; body}
let tdecls ds = T.VarDeclStm (T.DeclBlock ds)
let texpr value = T.ExprStm {expr = Some value}
let tblock stms = T.CompoundStm {stms}
let tret value = T.ReturnStm {ret = value}
let tprogram stms = stms @ [tret (tint 0)]

(* ------------------------------------------------------------------------
   Assertions
   ------------------------------------------------------------------------ *)
let expect_typed source expected =
  equal "program structure, order, identifiers, and type annotations"
    expected (Semant.typecheck_prog source)

let contains text sub =
  let n = String.length text and m = String.length sub in
  let rec go i = i + m <= n && (String.sub text i m = sub || go (i + 1)) in
  go 0

(* The program must be rejected with exactly one TypeError message per
   expected fragment, in order. Checking the exact count also verifies that
   an error does not cause follow-up errors. Only Semant.TypeError counts as
   a rejection: any other exception is a crash and makes the test fail. *)
let expect_errors fragments source =
  match Semant.typecheck_prog source with
  | _ -> fail "expected a TypeError, but the program was accepted"
  | exception Semant.TypeError messages ->
    let shown = String.concat " | " messages in
    if List.length messages <> List.length fragments then
      fail (Printf.sprintf "expected %d error(s), got %d: %s"
              (List.length fragments) (List.length messages) shown);
    List.iter2
      (fun fragment message ->
         if not (contains message fragment) then
           fail (Printf.sprintf "expected an error containing \"%s\", got: %s" fragment shown))
      fragments messages

let expect_error fragment source = expect_errors [fragment] source

(* ------------------------------------------------------------------------
   Tests
   ------------------------------------------------------------------------ *)
let declarations_tests = [
  "integer return", (fun () ->
    expect_typed [ret (int 42)] [tret (tint 42)]);
  "multiple declarations: order, inference, and preceding variables", (fun () ->
    expect_typed
      [decls [decl ~tp:ty_int "x" (int 2); decl "z" (int 5);
              decl "w" (bool true); decl "t" (bin (var "x") op_plus (var "z"))];
       ret (var "t")]
      [tdecls [tdecl "x" T.Int (tint 2); tdecl "z" T.Int (tint 5);
               tdecl "w" T.Bool (tbool true);
               tdecl "t" T.Int (tbin (tvar "x" T.Int) T.Plus (tvar "z" T.Int) T.Int)];
       tret (tvar "t" T.Int)]);
  "empty declaration block is preserved", (fun () ->
    expect_typed (program [decls []]) (tprogram [tdecls []]));
  "declaration block agrees with separate declarations", (fun () ->
    let ds = [decl "x" (int 2); decl "z" (bin (var "x") op_plus (int 5))] in
    let grouped = Semant.typecheck_prog [decls ds; ret (var "z")] in
    let separate = Semant.typecheck_prog [decls [List.hd ds];
                                         decls [List.nth ds 1]; ret (var "z")] in
    match grouped, separate with
    | [T.VarDeclStm (T.DeclBlock actual); grouped_return],
      [T.VarDeclStm (T.DeclBlock [first]); T.VarDeclStm (T.DeclBlock [second]); separate_return] ->
      equal "declarations" [first; second] actual;
      equal "return binding" separate_return grouped_return
    | _ -> fail "unexpected declaration layout");
  "explicit boolean declaration", (fun () ->
    expect_typed (program [decls [decl ~tp:ty_bool "b" (bool false)]])
      (tprogram [tdecls [tdecl "b" T.Bool (tbool false)]]));
  "initializer sees binding before shadowing", (fun () ->
    expect_typed
      [decls [decl "x" (int 3)];
       block [decls [decl ~tp:ty_bool "x" (bin (var "x") op_lt (int 5))]];
       ret (var "x")]
      [tdecls [tdecl "x" T.Int (tint 3)];
       tblock [tdecls [tdecl "x" T.Bool (tbin (tvar "x" T.Int) T.Lt (tint 5) T.Bool)]];
       tret (tvar "x" T.Int)]);
  "forward reference is rejected", (fun () ->
    expect_error "unbound identifier 'z'"
      (program [decls [decl "x" (var "z"); decl "z" (int 5)]]));
  "self reference without outer binding is rejected", (fun () ->
    expect_error "unbound identifier 'x'" (program [decls [decl "x" (var "x")]]));
  "declared type mismatch", (fun () ->
    expect_error "type mismatch in declaration of 'x': expected bool but found int"
      (program [decls [decl ~tp:ty_bool "x" (int 1)]]));
  "compound declaration cannot escape", (fun () ->
    expect_error "unbound identifier 'x'" [block [decls [decl "x" (int 1)]]; ret (var "x")]);
  "checking separate programs does not share bindings", (fun () ->
    ignore (Semant.typecheck_prog (program [decls [decl "x" (int 1)]]));
    expect_error "unbound identifier 'x'" [ret (var "x")])
]

let arithmetic_tests =
  List.map (fun (name, source_op, typed_op) ->
    "arithmetic annotation: " ^ name, (fun () ->
      expect_typed [ret (bin (int 8) source_op (int 2))]
        [tret (tbin (tint 8) typed_op (tint 2) T.Int)]))
    ["+", op_plus, T.Plus; "-", op_minus, T.Minus; "*", op_mul, T.Mul;
     "/", op_div, T.Div; "%", op_rem, T.Rem]

let comparison_tests =
  List.map (fun (name, source_op, typed_op) ->
    "comparison annotation: " ^ name, (fun () ->
      expect_typed (program [decls [decl "b" (bin (int 8) source_op (int 2))]])
        (tprogram [tdecls [tdecl "b" T.Bool (tbin (tint 8) typed_op (tint 2) T.Bool)]])))
    ["<", op_lt, T.Lt; "<=", op_le, T.Le; ">", op_gt, T.Gt;
     ">=", op_ge, T.Ge; "==", op_eq, T.Eq; "!=", op_neq, T.NEq]

let boolean_tests =
  List.map (fun (name, source_op, typed_op) ->
    "boolean annotation: " ^ name, (fun () ->
      expect_typed (program [decls [decl "b" (bin (bool true) source_op (bool false))]])
        (tprogram [tdecls [tdecl "b" T.Bool (tbin (tbool true) typed_op (tbool false) T.Bool)]])))
    ["&&", op_land, T.Land; "||", op_lor, T.Lor;
     "==", op_eq, T.Eq; "!=", op_neq, T.NEq]

let expression_tests = [
  "negation annotation", (fun () ->
    expect_typed [ret (unary op_neg (int 3))] [tret (tunary T.Neg (tint 3) T.Int)]);
  "logical not annotation", (fun () ->
    expect_typed (program [decls [decl "b" (unary op_lnot (bool true))]])
      (tprogram [tdecls [tdecl "b" T.Bool (tunary T.Lnot (tbool true) T.Bool)]]));
  "assignment annotates target and result", (fun () ->
    expect_typed [decls [decl "x" (int 1)]; expr (assign "x" (int 2)); ret (var "x")]
      [tdecls [tdecl "x" T.Int (tint 1)]; texpr (tassign "x" (tint 2) T.Int);
       tret (tvar "x" T.Int)]);
  "value-returning call annotation", (fun () ->
    expect_typed [ret (call "read_integer" [])]
      [tret (tcall "read_integer" [] (T.RetTyp T.Int))]);
  "void call statement annotation", (fun () ->
    expect_typed (program [expr (call "print_integer" [int 7])])
      (tprogram [texpr (tcall "print_integer" [tint 7] T.Void)]));
  "empty expression statement", (fun () ->
    expect_typed (program [empty_stm]) (tprogram [T.ExprStm {expr = None}]));
  "boolean arithmetic is rejected", (fun () ->
    expect_error "operator '+' cannot be applied to operands of type bool and int"
      [ret (bin (bool true) op_plus (int 1))]);
  "mixed-type equality is rejected", (fun () ->
    expect_error "operator '==' cannot be applied"
      (program [decls [decl "b" (bin (int 1) op_eq (bool true))]]));
  "integer logical operation is rejected", (fun () ->
    expect_error "operator '&&' cannot be applied"
      (program [decls [decl "b" (bin (int 1) op_land (int 2))]]));
  "invalid unary operand", (fun () ->
    expect_error "operator '-' cannot be applied to an operand of type bool"
      [ret (unary op_neg (bool true))]);
  "assignment type mismatch", (fun () ->
    expect_error "type mismatch in assignment: expected int but found bool"
      (program [decls [decl "x" (int 1)]; expr (assign "x" (bool true))]));
  "call argument type mismatch", (fun () ->
    expect_error "argument 1 of 'print_integer' should have type int but has type bool"
      (program [expr (call "print_integer" [bool true])]));
  "call arity mismatch", (fun () ->
    expect_error "function 'read_integer' expects 0 argument(s) but is given 1"
      [ret (call "read_integer" [int 1])]);
  "shadowed function cannot be called", (fun () ->
    expect_error "'print_integer' is a variable, not a function"
      (program [decls [decl "print_integer" (int 1)]; expr (call "print_integer" [int 1])]));
  "function identifier is not a variable", (fun () ->
    expect_error "function 'read_integer' cannot be used as a value" [ret (var "read_integer")]);
  "void initializer is rejected", (fun () ->
    expect_error "call to 'print_integer' returns void"
      (program [decls [decl "x" (call "print_integer" [int 1])]]));
  "void condition is rejected", (fun () ->
    expect_error "call to 'print_integer' returns void"
      (program [while_ (call "print_integer" [int 1]) (block [])]));
  "literal expression statement is rejected", (fun () ->
    expect_error "only assignments and function calls can be used as statements"
      (program [expr (int 1)]))
]

let branch_tests = [
  "if annotations and optional else", (fun () ->
    expect_typed (program [if_ (bin (int 1) op_lt (int 2)) (expr (call "print_integer" [int 1]))])
      (tprogram [T.IfThenElseStm {
        cond = tbin (tint 1) T.Lt (tint 2) T.Bool;
        thbr = texpr (tcall "print_integer" [tint 1] T.Void); elbro = None}]));
  "then declaration cannot escape", (fun () ->
    expect_error "unbound identifier 'x'"
      [if_ (bool true) (decls [decl "x" (int 1)]); ret (var "x")]);
  "then declaration unavailable in else", (fun () ->
    expect_error "unbound identifier 'x'"
      (program [if_ ~els:(expr (assign "x" (int 2))) (bool true) (decls [decl "x" (int 1)])]));
  "non-boolean if condition", (fun () ->
    expect_error "type mismatch in if condition: expected bool but found int"
      (program [if_ (int 1) (block [])]))
]

let loop_tests = [
  "while preserves boolean condition and control statements", (fun () ->
    expect_typed (program [while_ (bin (int 1) op_lt (int 2)) (block [continue_; break_])])
      (tprogram [T.WhileStm {cond = tbin (tint 1) T.Lt (tint 2) T.Bool;
        body = tblock [T.ContinueStm; T.BreakStm]}]));
  "while condition must be boolean", (fun () ->
    expect_error "type mismatch in while condition" (program [while_ (int 1) (block [])]));
  "break outside loops", (fun () ->
    expect_error "break statement outside of a loop" (program [break_]));
  "continue outside loops", (fun () ->
    expect_error "continue statement outside of a loop" (program [continue_]));
  "loop context does not escape", (fun () ->
    expect_error "continue statement outside of a loop"
      (program [while_ (bool true) break_; continue_]));
  "loop context does not cross branches", (fun () ->
    expect_error "break statement outside of a loop"
      (program [if_ ~els:break_ (bool true) (while_ (bool true) break_)]));
  "declarations inside loops retain loop context", (fun () ->
    expect_typed (program [while_ (bool true) (block [decls [decl "x" (int 1)]; continue_])])
      (tprogram [T.WhileStm {cond = tbool true;
        body = tblock [tdecls [tdecl "x" T.Int (tint 1)]; T.ContinueStm]}]));
  "while declaration cannot escape", (fun () ->
    expect_error "unbound identifier 'x'"
      [while_ (bool true) (decls [decl "x" (int 1)]); ret (var "x")]);
  "for with every optional clause absent", (fun () ->
    expect_typed (program [for_ break_])
      (tprogram [T.ForStm {init = None; cond = None; update = None; body = T.BreakStm}]));
  "empty for declaration block", (fun () ->
    expect_typed (program [for_ ~init:(A.FIDecl (dblock [])) break_])
      (tprogram [T.ForStm {init = Some (T.FIDecl (T.DeclBlock []));
        cond = None; update = None; body = T.BreakStm}]));
  "for declarations visible in condition, update, and body", (fun () ->
    expect_typed (program [for_
      ~init:(A.FIDecl (dblock [decl "i" (int 0); decl "limit" (bin (var "i") op_plus (int 3))]))
      ~cond:(bin (var "i") op_lt (var "limit"))
      ~update:(assign "i" (bin (var "i") op_plus (int 1)))
      (block [expr (call "print_integer" [var "i"]); continue_])])
      (tprogram [T.ForStm {
        init = Some (T.FIDecl (T.DeclBlock [tdecl "i" T.Int (tint 0);
          tdecl "limit" T.Int (tbin (tvar "i" T.Int) T.Plus (tint 3) T.Int)]));
        cond = Some (tbin (tvar "i" T.Int) T.Lt (tvar "limit" T.Int) T.Bool);
        update = Some (tassign "i" (tbin (tvar "i" T.Int) T.Plus (tint 1) T.Int) T.Int);
        body = tblock [texpr (tcall "print_integer" [tvar "i" T.Int] T.Void); T.ContinueStm]}]));
  "integer initializer and boolean update are permitted", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (int 7)) ~update:(bool true) break_])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tint 7)); cond = None;
        update = Some (tbool true); body = T.BreakStm}]));
  "value-returning calls in for clauses", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (call "read_integer" []))
      ~update:(call "read_integer" []) break_])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tcall "read_integer" [] (T.RetTyp T.Int)));
        cond = None; update = Some (tcall "read_integer" [] (T.RetTyp T.Int)); body = T.BreakStm}]));
  "for condition must be boolean", (fun () ->
    expect_error "type mismatch in for condition" (program [for_ ~cond:(int 1) break_]));
  "for declaration cannot escape", (fun () ->
    expect_error "unbound identifier 'i'"
      [for_ ~init:(A.FIDecl (dblock [decl "i" (int 0)])) break_; ret (var "i")]);
  "for shadowing restores outer binding", (fun () ->
    expect_typed [decls [decl "i" (int 7)];
      for_ ~init:(A.FIDecl (dblock [decl "i" (bool true)])) ~cond:(var "i") break_;
      ret (var "i")]
      [tdecls [tdecl "i" T.Int (tint 7)];
       T.ForStm {init = Some (T.FIDecl (T.DeclBlock [tdecl "i" T.Bool (tbool true)]));
         cond = Some (tvar "i" T.Bool); update = None; body = T.BreakStm};
       tret (tvar "i" T.Int)]);
  "body declarations unavailable in for update", (fun () ->
    expect_error "unbound identifier 'local'"
      (program [for_ ~update:(var "local") (decls [decl "local" (int 1)])]));
  "nested loops restore enclosing loop context", (fun () ->
    expect_typed (program [while_ (bool true) (block [for_ break_; continue_; break_])])
      (tprogram [T.WhileStm {cond = tbool true; body = tblock [
        T.ForStm {init = None; cond = None; update = None; body = T.BreakStm};
        T.ContinueStm; T.BreakStm]}]));
  "loop state does not cross programs", (fun () ->
    ignore (Semant.typecheck_prog (program [for_ break_]));
    expect_error "break statement outside of a loop" (program [break_]))
]

let program_tests = [
  "empty program is rejected", (fun () -> expect_error "program is empty" []);
  "missing final return", (fun () ->
    expect_error "program must end with a return statement" [decls []]);
  "return must produce int", (fun () ->
    expect_error "type mismatch in return value: expected int but found bool" [ret (bool true)])
]

(* For initialization/update expressions have no required result type, so
   well-typed void calls must be accepted there. *)
let void_clause_tests = [
  "for initializer permits a well-typed void call", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (call "print_integer" [int 1])) break_])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tcall "print_integer" [tint 1] T.Void));
        cond = None; update = None; body = T.BreakStm}]));
  "for update permits a well-typed void call", (fun () ->
    expect_typed (program [for_ ~update:(call "print_integer" [int 1]) continue_])
      (tprogram [T.ForStm {init = None; cond = None;
        update = Some (tcall "print_integer" [tint 1] T.Void); body = T.ContinueStm}]))
]

(* Phase 3: all errors are collected, without follow-up errors, and every
   message carries its source location. *)
let error_collection_tests = [
  "all errors are reported, in source order", (fun () ->
    expect_errors
      ["operator '+' cannot be applied"; "unbound identifier 'y'";
       "break statement outside of a loop"]
      (program [decls [decl "x" (bin (int 1) op_plus (bool true))];
                expr (assign "y" (int 2));
                break_]));
  "an ill-typed operand does not cause follow-up errors", (fun () ->
    expect_errors ["operator '+' cannot be applied"]
      [decls [decl "x" (bin (bool true) op_plus (int 1))];
       decls [decl "y" (bin (var "x") op_mul (int 2))];
       ret (var "y")]);
  "an unknown variable does not cause follow-up errors", (fun () ->
    expect_errors ["unbound identifier 'nope'"] [ret (bin (var "nope") op_plus (int 1))]);
  "arguments of an unknown function are still checked", (fun () ->
    expect_errors ["operator '+' cannot be applied"; "unbound identifier 'foo'"]
      (program [expr (call "foo" [bin (bool true) op_plus (int 1)])]));
  "error message includes the source location", (fun () ->
    let pos = {Lexing.pos_fname = "test.dlp"; pos_lnum = 3; pos_bol = 20; pos_cnum = 24} in
    let at = Location.make_location (pos, pos) in
    expect_error "test.dlp:3:5: error: unbound identifier 'nope'"
      [ret (A.Lval (A.Var (A.Ident {name = "nope"; loc = at})))])
]

let tests = declarations_tests @ arithmetic_tests @ comparison_tests @ boolean_tests
  @ expression_tests @ branch_tests @ loop_tests @ program_tests @ void_clause_tests
  @ error_collection_tests

let () =
  let passed = ref 0 and failed = ref 0 in
  List.iter (fun (name, run) ->
    try
      run ();
      incr passed;
      Printf.printf "PASS %s\n%!" name
    with error ->
      incr failed;
      let message = match error with
        | Test_failure message -> message
        | _ -> Printexc.to_string error
      in
      Printf.eprintf "FAIL %s: %s\n%!" name message) tests;
  Printf.printf "\n%d passed; %d failed; %d total.\n%!" !passed !failed (List.length tests);
  if !failed <> 0 then exit 1