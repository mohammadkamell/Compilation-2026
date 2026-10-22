(* Direct Ast.program -> Semant.typecheck_prog -> TypedAst.program tests.
   No parser, deserializer, backend, LLVM execution, or test framework. *)
module A = Ast
module T = TypedAst

exception Test_failure of string

let fail message = raise (Test_failure message)
let equal label expected actual =
  if expected <> actual then fail (label ^ ": unexpected typed AST")

(* Source-AST builders. *)
let id name = A.Ident {name}
let int n = A.Integer {int = Int64.of_int n}
let bool value = A.Boolean {bool = value}
let var name = A.Lval (A.Var (id name))
let bin left op right = A.BinOp {left; op; right}
let unary op operand = A.UnOp {op; operand}
let call name args = A.Call {fname = id name; args}
let assign name rhs = A.Assignment {lvl = A.Var (id name); rhs}
let decl ?tp name body = A.Declaration {name = id name; tp; body}
let decls ds = A.VarDeclStm (A.DeclBlock ds)
let expr value = A.ExprStm {expr = Some value}
let block stms = A.CompoundStm {stms}
let ret value = A.ReturnStm {ret = value}
let while_ cond body = A.WhileStm {cond; body}
let for_ ?init ?cond ?update body = A.ForStm {init; cond; update; body}
let program stms = stms @ [ret (int 0)]

(* Expected typed-AST builders. These do not call semantic analysis. *)
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

let expect_typed source expected =
  equal "program structure, order, identifiers, and type annotations"
    expected (Semant.typecheck_prog source)

(* Only the specified semantic exception counts as rejection. In particular,
   arbitrary crashes or a test's own assertion failure must not count as success. *)
let expect_failure message source =
  let result =
    try ignore (Semant.typecheck_prog source); None
    with Failure actual -> Some actual
  in
  match result with
  | Some actual when actual = message -> ()
  | Some actual -> fail ("expected Failure " ^ message ^ ", got " ^ actual)
  | None -> fail ("expected Failure " ^ message ^ ", but program was accepted")

let expect_type_error message source =
  let result =
    try ignore (Semant.typecheck_prog source); None
    with Semant.TypeError messages -> Some messages
  in
  match result with
  | Some [actual] when actual = message -> ()
  | Some actual -> fail ("unexpected TypeError: " ^ String.concat "; " actual)
  | None -> fail ("expected TypeError " ^ message ^ ", but program was accepted")

let declarations_tests = [
  "integer return", (fun () ->
    expect_typed [ret (int 42)] [tret (tint 42)]);
  "multiple declarations: order, inference, and preceding variables", (fun () ->
    expect_typed
      [decls [decl ~tp:A.Int "x" (int 2); decl "z" (int 5);
              decl "w" (bool true); decl "t" (bin (var "x") A.Plus (var "z"))];
       ret (var "t")]
      [tdecls [tdecl "x" T.Int (tint 2); tdecl "z" T.Int (tint 5);
               tdecl "w" T.Bool (tbool true);
               tdecl "t" T.Int (tbin (tvar "x" T.Int) T.Plus (tvar "z" T.Int) T.Int)];
       tret (tvar "t" T.Int)]);
  "empty declaration block is preserved", (fun () ->
    expect_typed (program [decls []]) (tprogram [tdecls []]));
  "declaration block agrees with separate declarations", (fun () ->
    let ds = [decl "x" (int 2); decl "z" (bin (var "x") A.Plus (int 5))] in
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
    expect_typed (program [decls [decl ~tp:A.Bool "b" (bool false)]])
      (tprogram [tdecls [tdecl "b" T.Bool (tbool false)]]));
  "initializer sees binding before shadowing", (fun () ->
    expect_typed
      [decls [decl "x" (int 3)];
       block [decls [decl ~tp:A.Bool "x" (bin (var "x") A.Lt (int 5))]];
       ret (var "x")]
      [tdecls [tdecl "x" T.Int (tint 3)];
       tblock [tdecls [tdecl "x" T.Bool (tbin (tvar "x" T.Int) T.Lt (tint 5) T.Bool)]];
       tret (tvar "x" T.Int)]);
  "forward reference is rejected", (fun () ->
    expect_failure "unbond: z"
      (program [decls [decl "x" (var "z"); decl "z" (int 5)]]));
  "self reference without outer binding is rejected", (fun () ->
    expect_failure "unbond: x" (program [decls [decl "x" (var "x")]]));
  "declared type mismatch", (fun () ->
    expect_failure "Variable has the wrong type"
      (program [decls [decl ~tp:A.Bool "x" (int 1)]]));
  "compound declaration cannot escape", (fun () ->
    expect_failure "unbond: x" [block [decls [decl "x" (int 1)]]; ret (var "x")]);
  "checking separate programs does not share bindings", (fun () ->
    ignore (Semant.typecheck_prog (program [decls [decl "x" (int 1)]]));
    expect_failure "unbond: x" [ret (var "x")])
]

let arithmetic_tests =
  List.map (fun (name, source_op, typed_op) ->
    "arithmetic annotation: " ^ name, (fun () ->
      expect_typed [ret (bin (int 8) source_op (int 2))]
        [tret (tbin (tint 8) typed_op (tint 2) T.Int)]))
    ["+", A.Plus, T.Plus; "-", A.Minus, T.Minus; "*", A.Mul, T.Mul;
     "/", A.Div, T.Div; "%", A.Rem, T.Rem]

let comparison_tests =
  List.map (fun (name, source_op, typed_op) ->
    "comparison annotation: " ^ name, (fun () ->
      expect_typed (program [decls [decl "b" (bin (int 8) source_op (int 2))]])
        (tprogram [tdecls [tdecl "b" T.Bool (tbin (tint 8) typed_op (tint 2) T.Bool)]])))
    ["<", A.Lt, T.Lt; "<=", A.Le, T.Le; ">", A.Gt, T.Gt;
     ">=", A.Ge, T.Ge; "==", A.Eq, T.Eq; "!=", A.NEq, T.NEq]

let boolean_tests =
  List.map (fun (name, source_op, typed_op) ->
    "boolean annotation: " ^ name, (fun () ->
      expect_typed (program [decls [decl "b" (bin (bool true) source_op (bool false))]])
        (tprogram [tdecls [tdecl "b" T.Bool (tbin (tbool true) typed_op (tbool false) T.Bool)]])))
    ["&&", A.Land, T.Land; "||", A.Lor, T.Lor;
     "==", A.Eq, T.Eq; "!=", A.NEq, T.NEq]

let expression_tests = [
  "negation annotation", (fun () ->
    expect_typed [ret (unary A.Neg (int 3))] [tret (tunary T.Neg (tint 3) T.Int)]);
  "logical not annotation", (fun () ->
    expect_typed (program [decls [decl "b" (unary A.Lnot (bool true))]])
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
    expect_typed (program [A.ExprStm {expr = None}])
      (tprogram [T.ExprStm {expr = None}]));
  "boolean arithmetic is rejected", (fun () ->
    expect_failure "type error in Binop" [ret (bin (bool true) A.Plus (int 1))]);
  "mixed-type equality is rejected", (fun () ->
    expect_failure "type error in Binop"
      (program [decls [decl "b" (bin (int 1) A.Eq (bool true))]]));
  "integer logical operation is rejected", (fun () ->
    expect_failure "type error in Binop"
      (program [decls [decl "b" (bin (int 1) A.Land (int 2))]]));
  "invalid unary operand", (fun () ->
    expect_failure "type error in UnOp" [ret (unary A.Neg (bool true))]);
  "assignment type mismatch", (fun () ->
    expect_failure "type mismatch in assignment"
      (program [decls [decl "x" (int 1)]; expr (assign "x" (bool true))]));
  "call argument type mismatch", (fun () ->
    expect_failure "error: arguments do not match function parameters"
      (program [expr (call "print_integer" [bool true])]));
  "call arity mismatch", (fun () ->
    expect_failure "error in Call" [ret (call "read_integer" [int 1])]);
  "shadowed function cannot be called", (fun () ->
    expect_failure "only functions can be called"
      (program [decls [decl "print_integer" (int 1)]; expr (call "print_integer" [int 1])]));
  "function identifier is not a variable", (fun () ->
    expect_failure "A function cannot be used as a value" [ret (var "read_integer")]);
  "void initializer is rejected", (fun () ->
    expect_failure "void call cannot be used as a value"
      (program [decls [decl "x" (call "print_integer" [int 1])]]));
  "void condition is rejected", (fun () ->
    expect_failure "void call cannot be used as a value"
      (program [while_ (call "print_integer" [int 1]) (block [])]));
  "literal expression statement is rejected", (fun () ->
    expect_failure "only assign and calls are valid statements" (program [expr (int 1)]))
]

let branch_tests = [
  "if annotations and optional else", (fun () ->
    expect_typed (program [A.IfThenElseStm {
      cond = bin (int 1) A.Lt (int 2); thbr = expr (call "print_integer" [int 1]); elbro = None}])
      (tprogram [T.IfThenElseStm {
        cond = tbin (tint 1) T.Lt (tint 2) T.Bool;
        thbr = texpr (tcall "print_integer" [tint 1] T.Void); elbro = None}]));
  "then declaration cannot escape", (fun () ->
    expect_failure "unbond: x" [A.IfThenElseStm {
      cond = bool true; thbr = decls [decl "x" (int 1)]; elbro = None}; ret (var "x")]);
  "then declaration unavailable in else", (fun () ->
    expect_failure "unbond: x" (program [A.IfThenElseStm {
      cond = bool true; thbr = decls [decl "x" (int 1)]; elbro = Some (expr (assign "x" (int 2)))}]));
  "non-boolean if condition", (fun () ->
    expect_failure "type mismatch" (program [A.IfThenElseStm {
      cond = int 1; thbr = block []; elbro = None}]))
]

let loop_tests = [
  "while preserves boolean condition and control statements", (fun () ->
    expect_typed (program [while_ (bin (int 1) A.Lt (int 2))
      (block [Ast.ContinueStm; Ast.BreakStm])])
      (tprogram [T.WhileStm {cond = tbin (tint 1) T.Lt (tint 2) T.Bool;
        body = tblock [T.ContinueStm; T.BreakStm]}]));
  "while condition must be boolean", (fun () ->
    expect_failure "type mismatch" (program [while_ (int 1) (block [])]));
  "break outside loops", (fun () ->
    expect_type_error "break statement outside of a loop." (program [A.BreakStm]));
  "continue outside loops", (fun () ->
    expect_type_error "continue statement outside of a loop." (program [A.ContinueStm]));
  "loop context does not escape", (fun () ->
    expect_type_error "continue statement outside of a loop."
      (program [while_ (bool true) A.BreakStm; A.ContinueStm]));
  "loop context does not cross branches", (fun () ->
    expect_type_error "break statement outside of a loop." (program [A.IfThenElseStm {
      cond = bool true; thbr = while_ (bool true) A.BreakStm; elbro = Some A.BreakStm}]));
  "declarations inside loops retain loop context", (fun () ->
    expect_typed (program [while_ (bool true) (block [decls [decl "x" (int 1)]; A.ContinueStm])])
      (tprogram [T.WhileStm {cond = tbool true;
        body = tblock [tdecls [tdecl "x" T.Int (tint 1)]; T.ContinueStm]}]));
  "while declaration cannot escape", (fun () ->
    expect_failure "unbond: x" [while_ (bool true) (decls [decl "x" (int 1)]); ret (var "x")]);
  "for with every optional clause absent", (fun () ->
    expect_typed (program [for_ A.BreakStm])
      (tprogram [T.ForStm {init = None; cond = None; update = None; body = T.BreakStm}]));
  "empty for declaration block", (fun () ->
    expect_typed (program [for_ ~init:(A.FIDecl (A.DeclBlock [])) A.BreakStm])
      (tprogram [T.ForStm {init = Some (T.FIDecl (T.DeclBlock []));
        cond = None; update = None; body = T.BreakStm}]));
  "for declarations visible in condition, update, and body", (fun () ->
    expect_typed (program [for_
      ~init:(A.FIDecl (A.DeclBlock [decl "i" (int 0); decl "limit" (bin (var "i") A.Plus (int 3))]))
      ~cond:(bin (var "i") A.Lt (var "limit"))
      ~update:(assign "i" (bin (var "i") A.Plus (int 1)))
      (block [expr (call "print_integer" [var "i"]); A.ContinueStm])])
      (tprogram [T.ForStm {
        init = Some (T.FIDecl (T.DeclBlock [tdecl "i" T.Int (tint 0);
          tdecl "limit" T.Int (tbin (tvar "i" T.Int) T.Plus (tint 3) T.Int)]));
        cond = Some (tbin (tvar "i" T.Int) T.Lt (tvar "limit" T.Int) T.Bool);
        update = Some (tassign "i" (tbin (tvar "i" T.Int) T.Plus (tint 1) T.Int) T.Int);
        body = tblock [texpr (tcall "print_integer" [tvar "i" T.Int] T.Void); T.ContinueStm]}]));
  "integer initializer and boolean update are permitted", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (int 7)) ~update:(bool true) A.BreakStm])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tint 7)); cond = None;
        update = Some (tbool true); body = T.BreakStm}]));
  "value-returning calls in for clauses", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (call "read_integer" []))
      ~update:(call "read_integer" []) A.BreakStm])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tcall "read_integer" [] (T.RetTyp T.Int)));
        cond = None; update = Some (tcall "read_integer" [] (T.RetTyp T.Int)); body = T.BreakStm}]));
  "for condition must be boolean", (fun () ->
    expect_failure "type mismatch" (program [for_ ~cond:(int 1) A.BreakStm]));
  "for declaration cannot escape", (fun () ->
    expect_failure "unbond: i" [for_ ~init:(A.FIDecl (A.DeclBlock [decl "i" (int 0)])) A.BreakStm;
                               ret (var "i")]);
  "for shadowing restores outer binding", (fun () ->
    expect_typed [decls [decl "i" (int 7)];
      for_ ~init:(A.FIDecl (A.DeclBlock [decl "i" (bool true)])) ~cond:(var "i") A.BreakStm;
      ret (var "i")]
      [tdecls [tdecl "i" T.Int (tint 7)];
       T.ForStm {init = Some (T.FIDecl (T.DeclBlock [tdecl "i" T.Bool (tbool true)]));
         cond = Some (tvar "i" T.Bool); update = None; body = T.BreakStm};
       tret (tvar "i" T.Int)]);
  "body declarations unavailable in for update", (fun () ->
    expect_failure "unbond: local"
      (program [for_ ~update:(var "local") (decls [decl "local" (int 1)])]));
  "nested loops restore enclosing loop context", (fun () ->
    expect_typed (program [while_ (bool true) (block [for_ A.BreakStm; A.ContinueStm; A.BreakStm])])
      (tprogram [T.WhileStm {cond = tbool true; body = tblock [
        T.ForStm {init = None; cond = None; update = None; body = T.BreakStm};
        T.ContinueStm; T.BreakStm]}]));
  "loop state does not cross programs", (fun () ->
    ignore (Semant.typecheck_prog (program [for_ A.BreakStm]));
    expect_type_error "break statement outside of a loop." (program [A.BreakStm]))
]

let program_tests = [
  "empty program is rejected", (fun () -> expect_failure "program is empty" []);
  "missing final return", (fun () ->
    expect_failure "program must have a return statement" [decls []]);
  "return must produce int", (fun () -> expect_failure "type mismatch" [ret (bool true)])
]

(* Specification regressions: for initialization/update expressions have no
   required result type. These tests intentionally require void calls to be
   accepted, rather than hiding the current implementation's rejection. *)
let void_clause_tests = [
  "for initializer permits a well-typed void call", (fun () ->
    expect_typed (program [for_ ~init:(A.FIExpr (call "print_integer" [int 1])) A.BreakStm])
      (tprogram [T.ForStm {init = Some (T.FIExpr (tcall "print_integer" [tint 1] T.Void));
        cond = None; update = None; body = T.BreakStm}]));
  "for update permits a well-typed void call", (fun () ->
    expect_typed (program [for_ ~update:(call "print_integer" [int 1]) A.ContinueStm])
      (tprogram [T.ForStm {init = None; cond = None;
        update = Some (tcall "print_integer" [tint 1] T.Void); body = T.ContinueStm}]))
]

let tests = declarations_tests @ arithmetic_tests @ comparison_tests @ boolean_tests
  @ expression_tests @ branch_tests @ loop_tests @ program_tests @ void_clause_tests

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
