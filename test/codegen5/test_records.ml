module T = TypedAst
let sym = Symbol.symbol
let id n = T.Ident {sym = sym n}
let int n = T.Integer {int = Int64.of_int n}
let print e = T.ExprStm {expr = Some (T.Call {fname = id "print_integer"; args = [e]; tp = T.Void})}
let cmp l op r = T.BinOp {left = l; op; right = r; tp = T.Bool}
let if_print cond = T.IfThenElseStm {cond; thbr = print (int 1); elbro = Some (print (int 0))}

let rn = T.RecordName {sym = sym "Pair"}
let fn s = T.FieldName {sym = sym s}
let pair_t = T.Record rn
let var x t = T.Lval (T.Var {ident = id x; tp = t})
let fld_lv e f t = T.Fld {rcrd = e; recname = rn; field = fn f; tp = t}
let fld e f t = T.Lval (fld_lv e f t)
let decl x t e = T.VarDeclStm (T.DeclBlock [T.Declaration {name = id x; tp = t; body = e}])

(* record Pair { first: int; next: Pair; }
   int main() {
     var p = new Pair { next = nil; first = 7; };      fields out of order
     var q = new Pair { first = 8; next = p; };
     print_integer(q.first);                            8
     print_integer(q.next.first);                       7
     p.first = 42;
     print_integer(q.next.first);                       42  (q.next and p are the same record)
     print_integer(q == q ? 1 : 0);                     1
     print_integer(p == nil ? 1 : 0);                   0
     print_integer(p.next == nil ? 1 : 0);              1
     return 0;
   } *)
let prog : T.program = [
  T.RecDecl {recname = rn; fields = [(fn "first", T.Int); (fn "next", pair_t)]};
  T.FunDecl {
    ret = T.RetTyp T.Int; funname = id "main"; params = [];
    body = [
      decl "p" pair_t (T.Rcrd {recname = rn; fields = [(fn "next", T.Nil); (fn "first", int 7)]});
      decl "q" pair_t (T.Rcrd {recname = rn; fields = [(fn "first", int 8); (fn "next", var "p" pair_t)]});
      print (fld (var "q" pair_t) "first" T.Int);
      print (fld (fld (var "q" pair_t) "next" pair_t) "first" T.Int);
      T.ExprStm {expr = Some (T.Assignment {lvl = fld_lv (var "p" pair_t) "first" T.Int;
                                            rhs = int 42; tp = T.Int})};
      print (fld (fld (var "q" pair_t) "next" pair_t) "first" T.Int);
      if_print (cmp (var "q" pair_t) T.Eq (var "q" pair_t));
      if_print (cmp (var "p" pair_t) T.Eq T.Nil);
      if_print (cmp (fld (var "p" pair_t) "next" pair_t) T.Eq T.Nil);
      T.ReturnStm {ret = Some (int 0)} ] } ]

let () = print_string (Ll.string_of_prog (Codegen.codegen_prog prog))