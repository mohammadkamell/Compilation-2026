module Sym = Symbol
module PBox = PrintBox
open TypedAst

let typ_to_string = function
| Int -> "int"
| Bool -> "bool"
| ErrorType -> "'type error'"

let rettyp_to_string = function
| Void -> "void"
| RetTyp tp -> typ_to_string tp

(* producing trees for pretty printing *)
let ident_to_tree (Ident {sym}) = Pretty.make_ident_line (Sym.name sym)

let typ_to_tree tp =
  match tp with
  | Int -> Pretty.make_typ_line "Int"
  | Bool -> Pretty.make_typ_line "Bool"
  | ErrorType -> PBox.line_with_style (PBox.Style.set_bg_color PBox.Style.Red PBox.Style.default) "ErrorType"

let rettyp_to_tree = function
| Void -> Pretty.make_typ_line "Void"
| RetTyp tp -> typ_to_tree tp

let binop_to_tree op =
  match op with
  | Plus -> Pretty.make_keyword_line "PLUS"
  | Minus -> Pretty.make_keyword_line "Minus"
  | Mul -> Pretty.make_keyword_line "Mul"
  | Div -> Pretty.make_keyword_line "Div"
  | Rem -> Pretty.make_keyword_line "Rem"
  | Lt -> Pretty.make_keyword_line "Lt"
  | Le -> Pretty.make_keyword_line "Le"
  | Gt -> Pretty.make_keyword_line "Gt"
  | Ge -> Pretty.make_keyword_line "Ge"
  | Lor -> Pretty.make_keyword_line "Lor"
  | Land -> Pretty.make_keyword_line "Land"
  | Eq -> Pretty.make_keyword_line "Eq"
  | NEq -> Pretty.make_keyword_line "NEq"

let unop_to_tree op =
  match op with
  | Neg -> Pretty.make_keyword_line "Neg"
  | Lnot -> Pretty.make_keyword_line "Lnot"

let rec expr_to_tree e =
  match e with
  | Integer {int; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "IntLit("; PBox.line (Int64.to_string int); Pretty.make_info_node_line ")"]
  | Boolean {bool; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "BooleanLit("; Pretty.make_keyword_line (if bool then "true" else "false"); Pretty.make_info_node_line ")"]
  | BinOp {left; op; right; tp; _} -> PBox.tree (Pretty.make_info_node_line "BinOp") [typ_to_tree tp; expr_to_tree left; binop_to_tree op; expr_to_tree right]
  | UnOp {op; operand; tp; _} -> PBox.tree (Pretty.make_info_node_line "UnOp") [typ_to_tree tp; unop_to_tree op; expr_to_tree operand]
  | Lval l -> PBox.tree (Pretty.make_info_node_line "Lval") [lval_to_tree l]
  | Assignment {lvl; rhs; tp; _} -> PBox.tree (Pretty.make_info_node_line "Assignment") [typ_to_tree tp; lval_to_tree lvl; expr_to_tree rhs]
  | Call {fname; args; tp; _} ->
    PBox.tree (Pretty.make_info_node_line "Call")
      [rettyp_to_tree tp; 
      PBox.hlist ~bars:false [Pretty.make_info_node_line "FunName: "; ident_to_tree fname];
        PBox.tree (Pretty.make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
and lval_to_tree l =
  match l with
  | Var {ident; tp} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "Var("; ident_to_tree ident; Pretty.make_info_node_line ")"; PBox.line " : "; typ_to_tree tp;]

let rec statement_to_tree c =
  match c with
  | VarDeclStm {name; tp; body; _} ->
    PBox.tree (Pretty.make_keyword_line "VarDeclStm")
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Ident: "; ident_to_tree name]; 
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; typ_to_tree tp];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Body: "; expr_to_tree body]]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (Pretty.make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [Pretty.make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [Pretty.make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
        match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [Pretty.make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | CompoundStm {stms; _} -> PBox.tree (Pretty.make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [Pretty.make_keyword_line "ReturnValStm: "; expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let program_to_tree prg =
  PBox.tree (Pretty.make_info_node_line "Program") (statement_seq_to_forest prg)