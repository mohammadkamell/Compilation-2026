(* Typed AST for Dolphin, phase 5.
   Contract between semantic analysis (producer) and code generation
   (consumer). Changes compared to phase 4 are marked with (* phase 5 *). *)
module Sym = Symbol

type ident = Ident of {sym : Sym.symbol}

type recordname = RecordName of {sym : Sym.symbol}           (* phase 5 *)
type fieldname = FieldName of {sym : Sym.symbol}             (* phase 5 *)

type typ =
  | Int
  | Bool
  | ErrorType
  | Str                                                      (* phase 5 *)
  | Byte                                                     (* phase 5 *)
  | Array of typ              (* element type *)             (* phase 5 *)
  | Record of recordname      (* user-defined or reserved *) (* phase 5 *)
  | NilType                   (* the type of nil; compatible with every
                                 record and array type *)    (* phase 5 *)

type rettyp = Void | RetTyp of typ

type binop = | Plus | Minus | Mul | Div | Rem | Lt
  | Le | Gt | Ge | Lor | Land | Eq | NEq

type unop = | Neg | Lnot

type expr =
| Integer of {int : int64}
| Boolean of {bool : bool}
| String of {string : string}  (* escape sequences already resolved *) (* phase 5 *)
| BinOp of {left : expr; op : binop; right : expr; tp : typ}
| UnOp of {op : unop; operand : expr; tp : typ}
| LengthOf of {expr : expr}     (* argument is a string or an array *)  (* phase 5 *)
| Lval of lval
| Assignment of {lvl : lval; rhs : expr; tp : typ}
| Rcrd of {recname : recordname; fields : (fieldname * expr) list}
    (* fields in source order, i.e. the order of evaluation *)        (* phase 5 *)
| Arr of {elemtp : typ; len : expr}                                  (* phase 5 *)
| Nil                                                                (* phase 5 *)
| Call of {fname : ident; args : expr list; tp : rettyp}
| CommaExpr of {left : expr; right : expr; tp : typ}
and lval =
| Var of {ident : ident; tp : typ}
| Idx of {arr : expr; index : expr; tp : typ}  (* tp: element type *) (* phase 5 *)
| Fld of {rcrd : expr; recname : recordname; field : fieldname; tp : typ}
    (* recname: the record type of rcrd; tp: the type of the field *) (* phase 5 *)

type single_declaration = Declaration of {name : ident; tp : typ; body : expr}

type declaration_block = DeclBlock of single_declaration list

type for_init =
| FIDecl of declaration_block
| FIExpr of expr

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
| ReturnStm of {ret : expr option}

type param = Param of {paramname : ident; typ : typ}

type funtype = FunTyp of {ret : rettyp; params : param list}

type fundecl = {
  ret : rettyp;
  funname : ident;
  params : param list;
  body : statement list;
}

(* Fields in declaration order: the i-th field is field i of the LLVM struct. *)
type recdecl = {                                                     (* phase 5 *)
  recname : recordname;
  fields : (fieldname * typ) list;
}

type program_fragment =
  | FunDecl of fundecl
  | RecDecl of recdecl                                               (* phase 5 *)

type program = program_fragment list
