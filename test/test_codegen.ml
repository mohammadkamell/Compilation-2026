module T = TypedAst

let test_neg_mul : T.program = [
  T.ReturnStm {
    ret = T.BinOp {
      left  = T.UnOp {
                op    = T.Neg;
                operand = T.Integer {int = 5L};
                tp    = T.Int };
      op    = T.Mul;
      right = T.Integer {int = 5L};
      tp    = T.Int }
  }
]

let test_call_print : T.program = [
  T.ExprStm {expr = Some (T.Call {
    fname = T.Ident {sym = Symbol.symbol "print_integer"};
    args  = [ T.BinOp {
                left  = T.UnOp {op = T.Neg; operand = T.Integer {int = 5L}; tp = T.Int};
                op    = T.Mul;
                right = T.Integer {int = 30L};
                tp    = T.Int } ];
    tp    = T.Void })};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]



let test_read_mul : T.program = [
  T.ExprStm {expr = Some (T.Call {
    fname = T.Ident {sym = Symbol.symbol "print_integer"};
    args  = [ T.BinOp {
                left  = T.Call {
                          fname = T.Ident {sym = Symbol.symbol "read_integer"};
                          args  = [];
                          tp    = T.RetTyp T.Int };
                op    = T.Mul;
                right = T.Integer {int = 2L};
                tp    = T.Int } ];
    tp    = T.Void })};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]

let test_vardecl : T.program = [
  T.VarDeclStm {name = T.Ident {sym = Symbol.symbol "x"}; tp = T.Int; body = T.Integer {int = 10L}};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


(* var x = 10; print_integer(x * 2); return 0;   → prints 20 *)
let test_lval : T.program = [
  T.VarDeclStm {
    name = T.Ident {sym = Symbol.symbol "x"};
    tp   = T.Int;
    body = T.Integer {int = 10L} };
  T.ExprStm {expr = Some (T.Call {
    fname = T.Ident {sym = Symbol.symbol "print_integer"};
    args  = [ T.BinOp {
                left  = T.Lval (T.Var {ident = T.Ident {sym = Symbol.symbol "x"}; tp = T.Int});
                op    = T.Mul;
                right = T.Integer {int = 2L};
                tp    = T.Int } ];
    tp    = T.Void })};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


let x_sym = Symbol.symbol "x"
let x_lval = T.Var {ident = T.Ident {sym = x_sym}; tp = T.Int}


(* var x = 10; x = x * 3; print_integer(x); return 0 (should be 30)*)
let test_assign : T.program = [
  T.VarDeclStm {name = T.Ident {sym = x_sym}; tp = T.Int; body = T.Integer {int = 10L}};
  T.ExprStm {expr = Some (T.Assignment {
    lvl = x_lval;
    rhs = T.BinOp {left = T.Lval x_lval; op = T.Mul; right = T.Integer {int = 3L}; tp = T.Int};
    tp  = T.Int })};
  T.ExprStm {expr = Some (T.Call {
    fname = T.Ident {sym = Symbol.symbol "print_integer"};
    args  = [T.Lval x_lval];
    tp    = T.Void })};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


let print_x = T.ExprStm {expr = Some (T.Call {
  fname = T.Ident {sym = Symbol.symbol "print_integer"};
  args  = [T.Lval x_lval];
  tp    = T.Void })}

let test_shadow : T.program = [
  T.VarDeclStm {name = T.Ident {sym = x_sym}; tp = T.Int; body = T.Integer {int = 10L}};
  T.CompoundStm {stms = [
    T.VarDeclStm {name = T.Ident {sym = x_sym}; tp = T.Int; body = T.Integer {int = 12L}};
    print_x ]};
  print_x;
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


(* var b = 3 < 5; var c = -1 < 5; var d = b == c; return 0;
   out.ll: icmp slt i64 3, 5 / icmp slt i64 %temp, 5 / icmp eq i1 ... *)
let test_cmp : T.program = [
  T.VarDeclStm {
    name = T.Ident {sym = Symbol.symbol "b"};
    tp   = T.Bool;
    body = T.BinOp {left = T.Integer {int = 3L}; op = T.Lt;
                    right = T.Integer {int = 5L}; tp = T.Bool} };
  T.VarDeclStm {
    name = T.Ident {sym = Symbol.symbol "c"};
    tp   = T.Bool;
    body = T.BinOp {left = T.UnOp {op = T.Neg; operand = T.Integer {int = 1L}; tp = T.Int};
                    op = T.Lt;
                    right = T.Integer {int = 5L}; tp = T.Bool} };
  T.VarDeclStm {
    name = T.Ident {sym = Symbol.symbol "d"};
    tp   = T.Bool;
    body = T.BinOp {left  = T.Lval (T.Var {ident = T.Ident {sym = Symbol.symbol "b"}; tp = T.Bool});
                    op    = T.Eq;
                    right = T.Lval (T.Var {ident = T.Ident {sym = Symbol.symbol "c"}; tp = T.Bool});
                    tp    = T.Bool} };
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


let print_int n = T.ExprStm {expr = Some (T.Call {
  fname = T.Ident {sym = Symbol.symbol "print_integer"};
  args  = [T.Integer {int = Int64.of_int n}];
  tp    = T.Void })}

(* if (read_integer() < 10) print_integer(1); else print_integer(2); return 0;*)
let test_if : T.program = [
  T.IfThenElseStm {
    cond = T.BinOp {
      left  = T.Call {fname = T.Ident {sym = Symbol.symbol "read_integer"}; args = []; tp = T.RetTyp T.Int};
      op    = T.Lt;
      right = T.Integer {int = 10L};
      tp    = T.Bool };
    thbr  = print_int 1;
    elbro = None};
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


(* if (read_integer() < 10) return 1; else return 2; return 0;
   input 3 → exit code 1, input 42 → exit code 2 *)
let test_if_return : T.program = [
  T.IfThenElseStm {
    cond = T.BinOp {
      left  = T.Call {fname = T.Ident {sym = Symbol.symbol "read_integer"}; args = []; tp = T.RetTyp T.Int};
      op    = T.Lt;
      right = T.Integer {int = 10L};
      tp    = T.Bool };
    thbr  = T.ReturnStm {ret = T.Integer {int = 1L}};
    elbro = Some (T.ReturnStm {ret = T.Integer {int = 2L}}) };
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


let read_int = T.Call {fname = T.Ident {sym = Symbol.symbol "read_integer"}; args = []; tp = T.RetTyp T.Int}
let print_e e = T.ExprStm {expr = Some (T.Call {
  fname = T.Ident {sym = Symbol.symbol "print_integer"}; args = [e]; tp = T.Void})}

(* print (100 / input); print(-7%3). input 4 should print 4 and -1*)
let test_div : T.program = [
  print_e (T.BinOp {left = T.Integer {int = 100L}; op = T.Div; right = read_int; tp = T.Int});
  print_e (T.BinOp {left = T.UnOp {op = T.Neg; operand = T.Integer {int = 7L}; tp = T.Int};
                    op = T.Rem; right = T.Integer {int = 3L}; tp = T.Int});
  T.ReturnStm {ret = T.Integer {int = 0L}}
]


let xs = Symbol.symbol "x"
let xv = T.Lval (T.Var {ident = T.Ident {sym = xs}; tp = T.Int})

(* input 0 must not give div by zero error*)
let test_and_short_circuit : T.program = [
  T.VarDeclStm {name = T.Ident {sym = xs}; tp = T.Int; body = read_int};
  T.IfThenElseStm {
    cond = T.BinOp {
      left  = T.BinOp {left = xv; op = T.NEq; right = T.Integer {int = 0L}; tp = T.Bool};
      op    = T.Land;
      right = T.BinOp {
                left  = T.BinOp {left = T.Integer {int = 10L}; op = T.Div; right = xv; tp = T.Int};
                op    = T.Gt;
                right = T.Integer {int = 1L};
                tp    = T.Bool };
      tp    = T.Bool };
    thbr  = print_int 1;
    elbro = Some (print_int 2) };
  T.ReturnStm {ret = T.Integer {int = 0L}}
]



(* Change current to test one of the above programs. To run use "make test". *)
let current = test_and_short_circuit

let () = print_string (Ll.string_of_prog (Codegen.codegen_prog current))
