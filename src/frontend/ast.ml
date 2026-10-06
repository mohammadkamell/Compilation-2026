(* -- Use this in your solution without modifications *)
type ident = Ident of {name : string;}

type typ = | Int | Bool

type binop = | Plus | Minus | Mul | Div | Rem | Lt 
  | Le | Gt | Ge | Lor | Land | Eq | NEq

type unop = | Neg | Lnot

type expr =
| Integer of {int : int64}
| Boolean of {bool : bool}
| BinOp of {left : expr; op : binop; right : expr}
| UnOp of {op : unop; operand : expr}
| Lval of lval
| Assignment of {lvl : lval; rhs : expr;}
| Call of {fname : ident; args : expr list}
and lval =
| Var of ident

type single_declaration = 
  Declaration of {name : ident; tp : typ option; body : expr}

type declaration_block = DeclBlock of single_declaration list

type for_init =
| FIExpr of expr
| FIDecl of declaration_block

type statement =
| VarDeclStm of declaration_block
| ExprStm of {expr : expr option}
| IfThenElseStm of {cond : expr; thbr : statement; elbro : statement option}
| WhileStm of {cond : expr; body : statement}
| ForStm of { init : for_init option
            ; cond : expr option
            ; update : expr option
            ; body : statement }
| BreakStm
| ContinueStm
| CompoundStm of {stms : statement list}
| ReturnStm of {ret : expr}

type program = statement list
