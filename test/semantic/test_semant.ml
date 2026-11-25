(* Direct Ast.program -> Semant.typecheck_prog -> TypedAst.program tests.
   No parser, deserializer, backend, LLVM execution, or test framework.

   Phase 3: source ASTs carry locations (dummy locations here), and semantic
   analysis collects all errors and raises Semant.TypeError with one message
   per error, in source order.

   Phase 4: the statements of each test are wrapped in int main() { ... }
   by the helper [typecheck] below. *)
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
let ret value = A.ReturnStm {ret = Some value; loc}
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

(* Phase 4: a program is a list of functions. These tests check a function
   body, so they wrap the statements in int main() { ... }. This matches the
   temporary semantic analysis, which only checks the body of main. *)
let typecheck stms =
  Semant.typecheck_prog
    [A.FunDecl {A.ret = A.RetTyp ty_int; A.funname = id "main";
                A.params = []; A.body = stms; A.loc = loc}]

let typecheck_body stms =
  match typecheck stms with
  | [T.FunDecl {T.body = body; _}] -> body
  | _ -> fail "expectet exactly one function"

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
let tret value = T.ReturnStm {ret = Some value} let tprogram stms = stms @ [tret (tint 0)] 
let tmain body =   [T.FunDecl { T.funname = T.Ident {sym = Symbol.symbol "main"};
ret = T.RetTyp T.Int;
params = [];
body }]

(* ------------------------------------------------------------------------
   Assertions
   ------------------------------------------------------------------------ *)
let expect_typed source expected = 
  equal "program structure, order, identifiers, and type annotations" 
  (tmain expected) (typecheck source)

let contains text sub =
  let n = String.length text and m = String.length sub in
  let rec go i = i + m <= n && (String.sub text i m = sub || go (i + 1)) in
  go 0

(* The program must be rejected with exactly one TypeError message per
   expected fragment, in order. Checking the exact count also verifies that
   an error does not cause follow-up errors. Only Semant.TypeError counts as
   a rejection: any other exception is a crash and makes the test fail. *)
let expect_errors fragments source =
  match typecheck source with
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
    let grouped = typecheck_body [decls ds; ret (var "z")] in     
    let separate = typecheck_body [decls [List.hd ds];decls [List.nth ds 1]; ret (var "z")] in
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
    ignore (typecheck (program [decls [decl "x" (int 1)]]));
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
    ignore (typecheck (program [for_ break_]));
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

let rec_id name = A.RecordName {name; loc}
let fld_id name = A.FieldName {name; loc}
let str s = A.String {string = s; loc}
let nil_ = A.Nil {loc}
let len_of e = A.LengthOf {expr = e; loc}
let new_arr tp len = A.Arr {tp; len; loc}
let new_rec r fields = A.Rcrd {rcrdtp = rec_id r; fields = List.map (fun (f, e) -> (fld_id f, e)) fields; loc}
let idx_lval arr index = A.Idx {arr; index; loc}
let fld_lval rcrd field = A.Fld {rcrd; field = fld_id field; loc}
let idx arr index = A.Lval (idx_lval arr index)
let fld rcrd field = A.Lval (fld_lval rcrd field)
let assign_lval lvl rhs = A.Assignment {lvl; rhs; loc}
let ty_str = A.Str {loc}
let ty_byte = A.Byte {loc}
let ty_arr tp = A.Array {typ = tp; loc}
let ty_rec r = A.Record {recordname = rec_id r}
let recdecl name fields =
  A.RecDecl {recname = rec_id name; fields = List.map (fun (f, tp) -> (fld_id f, tp)) fields; loc}
let fundecl ?(ret=A.RetTyp ty_int) name params body =
  A.FunDecl {ret; funname = id name; params = List.map (fun (p, tp) -> (id p, tp)) params; body; loc}

let expect_prog_typed prg expected =
  equal "phase 5 typed program" expected (Semant.typecheck_prog prg)

let expect_prog_errors fragments prg =
  match Semant.typecheck_prog prg with
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

let expect_prog_error fragment prg = expect_prog_errors [fragment] prg

let phase5_tests = [
  "mutually recursive records and out-of-order fields", (fun () ->
    let prg = [
      recdecl "T1" [("x", ty_int); ("t2", ty_rec "T2")];
      recdecl "T2" [("y", ty_int); ("t1", ty_rec "T1")];
      fundecl "main" [] [
        decls [decl ~tp:(ty_rec "T2") "a" (new_rec "T2" [("t1", nil_); ("y", int 10)])];
        ret (fld (var "a") "y")
      ]
    ] in
    let expected = [
      T.RecDecl {recname = T.RecordName {sym = Symbol.symbol "T1"};
                 fields = [(T.FieldName {sym = Symbol.symbol "x"}, T.Int);
                           (T.FieldName {sym = Symbol.symbol "t2"}, T.Record (T.RecordName {sym = Symbol.symbol "T2"}))]};
      T.RecDecl {recname = T.RecordName {sym = Symbol.symbol "T2"};
                 fields = [(T.FieldName {sym = Symbol.symbol "y"}, T.Int);
                           (T.FieldName {sym = Symbol.symbol "t1"}, T.Record (T.RecordName {sym = Symbol.symbol "T1"}))]};
      T.FunDecl {funname = tid "main"; ret = T.RetTyp T.Int; params = []; body = [
        tdecls [tdecl "a" (T.Record (T.RecordName {sym = Symbol.symbol "T2"}))
                  (T.Rcrd {recname = T.RecordName {sym = Symbol.symbol "T2"};
                           fields = [(T.FieldName {sym = Symbol.symbol "t1"}, T.Nil);
                                     (T.FieldName {sym = Symbol.symbol "y"}, tint 10)]})];
        tret (T.Lval (T.Fld {rcrd = tvar "a" (T.Record (T.RecordName {sym = Symbol.symbol "T2"}));
                             recname = T.RecordName {sym = Symbol.symbol "T2"};
                             field = T.FieldName {sym = Symbol.symbol "y"};
                             tp = T.Int}))
      ]}
    ] in
    expect_prog_typed prg expected);

  "empty record is allowed", (fun () ->
    let prg = [
      recdecl "Empty" [];
      fundecl "main" [] [decls [decl "e" (new_rec "Empty" [])]; ret (int 0)]
    ] in
    ignore (Semant.typecheck_prog prg));

  "reserved record name is rejected", (fun () ->
    expect_prog_error "record name 'stream' is reserved"
      [recdecl "stream" [("x", ty_int)]; fundecl "main" [] [ret (int 0)]]);

  "duplicate record name is rejected", (fun () ->
    expect_prog_error "record 'Pair' is declared more than once"
      [recdecl "Pair" [("x", ty_int)]; recdecl "Pair" [("y", ty_int)];
       fundecl "main" [] [ret (int 0)]]);

  "cannot instantiate reserved record with new", (fun () ->
    expect_error "cannot instantiate reserved record type 'stream' with 'new'"
      (program [decls [decl ~tp:(ty_rec "stream") "s" (new_rec "stream" [])]]));

  "duplicate field in record declaration is rejected", (fun () ->
    expect_prog_error "duplicate field 'x' in record 'Pair'"
      [recdecl "Pair" [("x", ty_int); ("x", ty_bool)];
       fundecl "main" [] [ret (int 0)]]);

  "unknown record field type is rejected once", (fun () ->
    expect_prog_error "unknown record type 'Ghost'"
      [recdecl "Wrapper" [("g", ty_rec "Ghost")]; fundecl "main" [] [ret (int 0)]]);

  "missing field init in new record is rejected", (fun () ->
    expect_prog_error "missing initialization for field 'y' of record 'Pair'"
      [recdecl "Pair" [("x", ty_int); ("y", ty_int)];
       fundecl "main" [] [decls [decl "p" (new_rec "Pair" [("x", int 1)])]; ret (int 0)]]);

  "duplicate and unknown field init are rejected", (fun () ->
    expect_prog_errors
      ["field 'x' initialized more than once"; "record 'Pair' has no field 'z'"]
      [recdecl "Pair" [("x", ty_int)];
       fundecl "main" [] [decls [decl "p" (new_rec "Pair" [("x", int 1); ("x", int 2); ("z", int 3)])];
                          ret (int 0)]]);

  "record equality with same type and nil succeeds; different records rejected", (fun () ->
    let valid_prg = [
      recdecl "Pair" [("x", ty_int)];
      fundecl "main" [] [
        decls [decl "p" (new_rec "Pair" [("x", int 1)]);
               decl ~tp:(ty_rec "Pair") "q" nil_;
               decl "b1" (bin (var "p") op_eq nil_);
               decl "b2" (bin nil_ op_neq (var "q"));
               decl "b3" (bin (var "p") op_eq (var "q"))];
        ret (int 0)
      ]
    ] in
    ignore (Semant.typecheck_prog valid_prg);
    expect_prog_error "operator '==' cannot be applied"
      [recdecl "A" [("x", ty_int)]; recdecl "B" [("x", ty_int)];
       fundecl "main" [] [
         decls [decl "a" (new_rec "A" [("x", int 1)]); decl "b" (new_rec "B" [("x", int 1)])];
         decls [decl "eq" (bin (var "a") op_eq (var "b"))];
         ret (int 0)
       ]]);

  "unannotated nil and nil field access are rejected", (fun () ->
    expect_error "cannot infer type of 'nil'" (program [decls [decl "x" nil_]]);
    expect_error "field access requires a record type, but found nil" [ret (fld nil_ "x")]);

  "record field assignment works", (fun () ->
    let prg = [
      recdecl "Box" [("v", ty_int)];
      fundecl "main" [] [
        decls [decl "b" (new_rec "Box" [("v", int 1)])];
        expr (assign_lval (fld_lval (var "b") "v") (int 99));
        ret (fld (var "b") "v")
      ]
    ] in
    ignore (Semant.typecheck_prog prg));

  "arrays: creation, indexing, assignment, length_of, equality", (fun () ->
    let prg = [
      fundecl "main" [] [
        decls [decl "arr" (new_arr ty_int (int 5));
               decl "n" (len_of (var "arr"));
               decl "same" (bin (var "arr") op_eq (var "arr"))];
        expr (assign_lval (idx_lval (var "arr") (int 0)) (int 42));
        ret (idx (var "arr") (int 0))
      ]
    ] in
    ignore (Semant.typecheck_prog prg));

  "array errors: non-int length, non-int index, indexing non-array, mismatched equality", (fun () ->
    expect_error "type mismatch in array length" (program [decls [decl "a" (new_arr ty_int (bool true))]]);
    expect_error "type mismatch in array index"
      (program [decls [decl "a" (new_arr ty_int (int 2))]; ret (idx (var "a") (bool false))]);
    expect_error "indexing requires an array type, but found int"
      (program [decls [decl "x" (int 1)]; ret (idx (var "x") (int 0))]);
    expect_error "operator '==' cannot be applied"
      (program [decls [decl "a" (new_arr ty_int (int 1)); decl "b" (new_arr ty_bool (int 1));
                       decl "c" (bin (var "a") op_eq (var "b"))]]));

  "strings: literals, comparisons, length_of, stdlib calls, arithmetic rejected", (fun () ->
    let prg = [
      fundecl "main" [] [
        decls [decl "s1" (str "hello");
               decl "s2" (call "string_concat" [var "s1"; str " world"]);
               decl "cmp" (bin (var "s1") op_lt (var "s2"));
               decl "len" (len_of (var "s2"));
               decl "b" (call "int_to_byte_unsigned" [int 65]);
               decl "out" (call "get_stdout" [])];
        expr (call "output_string" [var "s2"; var "out"]);
        ret (var "len")
      ]
    ] in
    ignore (Semant.typecheck_prog prg);
    expect_error "operator '+' cannot be applied to operands of type string and string"
      [ret (bin (str "a") op_plus (str "b"))];
    expect_error "'length_of' requires an array or string, but found int"
      [ret (len_of (int 5))];
    expect_error "unbound identifier 'compare_strings'"
      [ret (call "compare_strings" [str "a"; str "b"])])
]

let tests = declarations_tests @ arithmetic_tests @ comparison_tests @ boolean_tests
  @ expression_tests @ branch_tests @ loop_tests @ program_tests @ void_clause_tests
  @ error_collection_tests @ phase5_tests

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
