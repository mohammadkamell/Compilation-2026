module T = TypedAst
let sym = Symbol.symbol
let id n = T.Ident {sym = sym n}
let int n = T.Integer {int = Int64.of_int n}
let print e = T.ExprStm {expr = Some (T.Call {fname = id "print_integer"; args = [e]; tp = T.Void})}
let cmp l op r = T.BinOp {left = l; op; right = r; tp = T.Bool}
let add l r = T.BinOp {left = l; op = T.Plus; right = r; tp = T.Int}
let if_print cond = T.IfThenElseStm {cond; thbr = print (int 1); elbro = Some (print (int 0))}
let var x t = T.Lval (T.Var {ident = id x; tp = t})
let decl x t e = T.VarDeclStm (T.DeclBlock [T.Declaration {name = id x; tp = t; body = e}])
let idx_lv a i t = T.Idx {arr = a; index = i; tp = t}
let idx a i t = T.Lval (idx_lv a i t)
let assign lv e t = T.ExprStm {expr = Some (T.Assignment {lvl = lv; rhs = e; tp = t})}

let ints = T.Array T.Int
(* Expected output: 30 5 0 0 0 7 1 1 *)
let prog : T.program = [
  T.FunDecl {
    ret = T.RetTyp T.Int; funname = id "main"; params = [];
    body = [
      decl "a" ints (T.Arr {elemtp = T.Int; len = int 5});
      assign (idx_lv (var "a" ints) (int 0) T.Int) (int 10) T.Int;
      assign (idx_lv (var "a" ints) (int 4) T.Int) (int 20) T.Int;
      print (add (idx (var "a" ints) (int 0) T.Int) (idx (var "a" ints) (int 4) T.Int));
      print (T.LengthOf {expr = var "a" ints});
      print (idx (var "a" ints) (int 2) T.Int);
      decl "b" (T.Array T.Bool) (T.Arr {elemtp = T.Bool; len = int 3});
      if_print (idx (var "b" (T.Array T.Bool)) (int 1) T.Bool);
      decl "s" (T.Array T.Str) (T.Arr {elemtp = T.Str; len = int 2});
      print (T.LengthOf {expr = idx (var "s" (T.Array T.Str)) (int 0) T.Str});
      decl "c" ints (var "a" ints);
      assign (idx_lv (var "c" ints) (int 1) T.Int) (int 7) T.Int;
      print (idx (var "a" ints) (int 1) T.Int);
      if_print (cmp (var "a" ints) T.Eq (var "c" ints));
      decl "m" (T.Array ints) (T.Arr {elemtp = ints; len = int 2});
      if_print (cmp (idx (var "m" (T.Array ints)) (int 0) ints) T.Eq T.Nil);
      T.ReturnStm {ret = Some (int 0)} ] } ]

let () = print_string (Ll.string_of_prog (Codegen.codegen_prog prog))
