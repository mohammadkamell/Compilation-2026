module T = TypedAst
let sym = Symbol.symbol
let id n = T.Ident {sym = sym n}
let int n = T.Integer {int = Int64.of_int n}
let str s = T.String {string = s}
let print e = T.ExprStm {expr = Some (T.Call {fname = id "print_integer"; args = [e]; tp = T.Void})}
let cmp l op r = T.BinOp {left = l; op; right = r; tp = T.Bool}
let if_print cond = T.IfThenElseStm {cond; thbr = print (int 1); elbro = Some (print (int 0))}
let s_var = T.Lval (T.Var {ident = id "s"; tp = T.Str})

(* var s = "hello";
   print_integer(length_of(s));                       5
   print_integer("abc" < "abd" ? 1 : 0);              1
   print_integer(s == "hello" ? 1 : 0);               1  (by value, different pointers)
   print_integer("b" > "a" ? 1 : 0);                  1
   print_integer("x" != "x" ? 1 : 0);                 0
   return 0; *)
let prog : T.program = [
  T.FunDecl {
    ret = T.RetTyp T.Int; funname = id "main"; params = [];
    body = [
      T.VarDeclStm (T.DeclBlock [T.Declaration {name = id "s"; tp = T.Str; body = str "hello"}]);
      print (T.LengthOf {expr = s_var});
      if_print (cmp (str "abc") T.Lt (str "abd"));
      if_print (cmp s_var T.Eq (str "hello"));
      if_print (cmp (str "b") T.Gt (str "a"));
      if_print (cmp (str "x") T.NEq (str "x"));
      T.ReturnStm {ret = Some (int 0)} ] } ]

let () = print_string (Ll.string_of_prog (Codegen.codegen_prog prog))