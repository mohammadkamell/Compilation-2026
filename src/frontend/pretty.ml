(* -- Use this in your solution without modifications *)

module PBox = PrintBox

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

let ident_to_tree (Ast.Ident {name}) = make_ident_line name

let typ_to_tree tp =
  match tp with
  | Ast.Bool -> make_typ_line "Bool"
  | Ast.Int -> make_typ_line "Int"

let binop_to_tree op =
  match op with
  | Ast.Plus -> make_keyword_line "PLUS"
  | Ast.Minus -> make_keyword_line "Minus"
  | Ast.Mul -> make_keyword_line "Mul"
  | Ast.Div -> make_keyword_line "Div"
  | Ast.Rem -> make_keyword_line "Rem"
  | Ast.Lt -> make_keyword_line "Lt"
  | Ast.Le -> make_keyword_line "Le"
  | Ast.Gt -> make_keyword_line "Gt"
  | Ast.Ge -> make_keyword_line "Ge"
  | Ast.Lor -> make_keyword_line "Lor"
  | Ast.Land -> make_keyword_line "Land"
  | Ast.Eq -> make_keyword_line "Eq"
  | Ast.NEq -> make_keyword_line "NEq"

let unop_to_tree op =
  match op with
  | Ast.Neg -> make_keyword_line "Neg"
  | Ast.Lnot -> make_keyword_line "Lor"

let rec expr_to_tree e =
  match e with
  | Ast.Integer {int; _} -> PBox.hlist ~bars:false [make_info_node_line "IntLit("; PBox.line (Int64.to_string int); make_info_node_line ")"]
  | Ast.Boolean {bool; _} -> PBox.hlist ~bars:false [make_info_node_line "BooleanLit("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
  | Ast.BinOp {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
  | Ast.UnOp {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
  | Ast.Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
  | Ast.Assignment {lvl; rhs; _} -> PBox.tree (make_info_node_line "Assignment") [lval_to_tree lvl; expr_to_tree rhs]
  | Ast.Call {fname; args; _} ->
    PBox.tree (make_info_node_line "Call")
      [PBox.hlist ~bars:false [make_info_node_line "FunName: "; ident_to_tree fname];
       PBox.tree (make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
and lval_to_tree l =
  match l with
  | Ast.Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]

let single_declaration_to_tree (Ast.Declaration {name; tp; body; _}) =
  PBox.tree (make_keyword_line "Declaration")
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name];
    PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
    PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (Ast.DeclBlock declarations) =
PBox.tree (make_keyword_line "VarDecl")  (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
| Ast.FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
| Ast.FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]

let rec statement_to_tree c =
  match c with
  | Ast.VarDeclStm db -> PBox.hlist ~bars:false [PBox.line "DeclStm: "; declaration_block_to_tree db]
  | Ast.ExprStm {expr} -> PBox.hlist ~bars:false [make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | Ast.IfThenElseStm {cond; thbr; elbro} ->
    PBox.tree (make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | Ast.WhileStm {cond; body} ->
    PBox.tree (make_keyword_line "WhileStm")
      [PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | Ast.ForStm {init; cond; update; body} ->
    PBox.tree (make_keyword_line "ForStm")
      [PBox.hlist ~bars:false [make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
        PBox.hlist ~bars:false [make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | Ast.BreakStm -> make_keyword_line "BreakStm"
  | Ast.ContinueStm -> make_keyword_line "ContinueStm"
  | Ast.CompoundStm {stms} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | Ast.ReturnStm {ret} -> PBox.hlist ~bars:false [make_keyword_line "ReturnValStm: "; expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let program_to_tree prog =
  PBox.tree (make_info_node_line "Program") (statement_seq_to_forest prog)
