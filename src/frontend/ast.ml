(* -- Use this in your solution without modifications *)
module Loc = Location

type ident = Ident of {name : string; loc : Loc.location}

type typ =
| Int of {loc : Loc.location}
| Bool of {loc : Loc.location}

<<<<<<< Updated upstream
=======
type rettyp =
| Void of {loc : Loc.location}
| RetTyp of typ

>>>>>>> Stashed changes
type binop =
| Plus of {loc : Loc.location}
| Minus of {loc : Loc.location}
| Mul of {loc : Loc.location}
| Div of {loc : Loc.location}
| Rem of {loc : Loc.location}
| Lt of {loc : Loc.location}
| Le of {loc : Loc.location}
| Gt of {loc : Loc.location}
| Ge of {loc : Loc.location}
| Lor of {loc : Loc.location}
| Land of {loc : Loc.location}
| Eq of {loc : Loc.location}
| NEq of {loc : Loc.location}

type unop =
| Neg of {loc : Loc.location}
| Lnot of {loc : Loc.location}

type expr =
| Integer of {int : int64; loc : Loc.location}
| Boolean of {bool : bool; loc : Loc.location}
| BinOp of {left : expr; op : binop; right : expr; loc : Loc.location}
| UnOp of {op : unop; operand : expr; loc : Loc.location}
| Lval of lval
| Assignment of {lvl : lval; rhs : expr; loc : Loc.location}
| Call of {fname : ident; args : expr list; loc : Loc.location}
<<<<<<< Updated upstream
=======
| CommaExpr of {left : expr; right : expr; loc : Loc.location}
>>>>>>> Stashed changes
and lval =
| Var of ident

type single_declaration = Declaration of {name : ident; tp : typ option; body : expr; loc : Loc.location}

type declaration_block = DeclBlock of {declarations : single_declaration list; loc : Loc.location}

type for_init =
| FIExpr of expr
| FIDecl of declaration_block

type statement =
| VarDeclStm of declaration_block
| ExprStm of {expr : expr option; loc : Loc.location}
| IfThenElseStm of {cond : expr; thbr : statement; elbro : statement option; loc : Loc.location}
| WhileStm of {cond : expr; body : statement; loc : Loc.location}
| ForStm of {init : for_init option; cond : expr option; update : expr option; body : statement; loc : Loc.location}
| BreakStm of {loc : Loc.location}
| ContinueStm of {loc : Loc.location}
| CompoundStm of {stms : statement list; loc : Loc.location}
<<<<<<< Updated upstream
| ReturnStm of {ret : expr; loc : Loc.location}

type program = statement list
=======
| ReturnStm of {ret : expr option; loc : Loc.location}

type fundecl = {ret : rettyp; funname : ident; params : (ident * typ) list; body : statement list; loc : Loc.location}

type program_fragment = FunDecl of fundecl

type program = program_fragment list
>>>>>>> Stashed changes
