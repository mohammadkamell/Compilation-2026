module PBox = PrintBox
open Ast

(* producing trees for pretty printing *)
let typ_style = PBox.Style.fg_color PBox.Style.Green
let ident_style = PBox.Style.fg_color PBox.Style.Yellow
let fieldname_style = ident_style
let keyword_style = PBox.Style.fg_color PBox.Style.Blue

let info_node_style = PBox.Style.fg_color PBox.Style.Cyan

let make_typ_line name = PBox.line_with_style typ_style name
let make_fieldname_line name = PBox.line_with_style fieldname_style name
let make_ident_line name = PBox.line_with_style ident_style name
let make_keyword_line name = PBox.line_with_style keyword_style name

let make_info_node_line info = PBox.line_with_style info_node_style info

let ident_to_tree (Ident {name}) = make_ident_line name

let typ_to_tree tp =
  match tp with
  | Bool -> make_typ_line "Bool"
  | Int -> make_typ_line "Int"

let binop_to_tree op =
  match op with
  | Plus -> make_keyword_line "Plus"
  | Minus -> make_keyword_line "Minus"
  | Mul -> make_keyword_line "Mul"
  | Div -> make_keyword_line "Div"
  | Rem -> make_keyword_line "Rem"
  | Lt -> make_keyword_line "Lt"
  | Le -> make_keyword_line "Le"
  | Gt -> make_keyword_line "Gt"
  | Ge -> make_keyword_line "Ge"
  | Lor -> make_keyword_line "Lor"
  | Land -> make_keyword_line "Land"
  | Eq -> make_keyword_line "Eq"
  | NEq -> make_keyword_line "NEq"

let unop_to_tree op =
  match op with
  | Neg -> make_keyword_line "Neg"
  | Lnot -> make_keyword_line "Lnot"
  
let rec expr_to_tree e =
  match e with
  | Integer {int; _} -> PBox.hlist ~bars:false [make_info_node_line "IntLit("; PBox.line (Int64.to_string int); make_info_node_line ")"]
  | Boolean {bool; _} -> PBox.hlist ~bars:false [make_info_node_line "BooleanLit("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
  | BinOp {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
  | UnOp {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
  | Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
  | Assignment {lvl; rhs; _} -> PBox.tree (make_info_node_line "Assignment") [lval_to_tree lvl; expr_to_tree rhs]
  | Call {fname; args; _} ->
    PBox.tree (make_info_node_line "Call")
      [PBox.hlist ~bars:false [make_info_node_line "FunName: "; ident_to_tree fname];
       PBox.tree (make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
and lval_to_tree l =
  match l with
  | Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]

let rec statement_to_tree c =
  match c with
  | VarDeclStm {name; tp; body} -> PBox.tree (make_keyword_line "VarDeclStm") 
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
    PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | CompoundStm {stms; _} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [make_keyword_line "ReturnValStm: "; expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let program_to_tree prog = 
  PBox.tree (make_info_node_line "Program") (statement_seq_to_forest prog)