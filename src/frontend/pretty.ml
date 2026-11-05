<<<<<<< Updated upstream
(* -- Use this in your solution without modifications *)
=======
(* Pretty printer for ASTs (module Pretty).
   Based on the version released for Phase 3, extended for Phase 4 with
   comma expressions, optional return values, return types and functions. *)
>>>>>>> Stashed changes

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

let ident_to_tree (Ident {name; _}) = make_ident_line name

let typ_to_tree tp =
  match tp with
  | Bool _ -> make_typ_line "Bool"
  | Int _ -> make_typ_line "Int"

<<<<<<< Updated upstream
let binop_to_tree op =
    match op with
    | Plus _ -> make_keyword_line "PLUS"
    | Minus _ -> make_keyword_line "Minus"
    | Mul _ -> make_keyword_line "Mul"
    | Div _ -> make_keyword_line "Div"
    | Rem _ -> make_keyword_line "Rem"
    | Lt _ -> make_keyword_line "Lt"
    | Le _ -> make_keyword_line "Le"
    | Gt _ -> make_keyword_line "Gt"
    | Ge _ -> make_keyword_line "Ge"
    | Lor _ -> make_keyword_line "Lor"
    | Land _ -> make_keyword_line "Land"
    | Eq _ -> make_keyword_line "Eq"
    | NEq _ -> make_keyword_line "NEq"
  
=======
(* Phase 4: return types *)
let rettyp_to_tree rt =
  match rt with
  | Void _ -> make_typ_line "Void"
  | RetTyp tp -> typ_to_tree tp

let binop_to_tree op =
  match op with
  | Plus _ -> make_keyword_line "PLUS"
  | Minus _ -> make_keyword_line "Minus"
  | Mul _ -> make_keyword_line "Mul"
  | Div _ -> make_keyword_line "Div"
  | Rem _ -> make_keyword_line "Rem"
  | Lt _ -> make_keyword_line "Lt"
  | Le _ -> make_keyword_line "Le"
  | Gt _ -> make_keyword_line "Gt"
  | Ge _ -> make_keyword_line "Ge"
  | Lor _ -> make_keyword_line "Lor"
  | Land _ -> make_keyword_line "Land"
  | Eq _ -> make_keyword_line "Eq"
  | NEq _ -> make_keyword_line "NEq"

>>>>>>> Stashed changes
let unop_to_tree op =
  match op with
  | Neg _ -> make_keyword_line "Neg"
  | Lnot _ -> make_keyword_line "Lnot"
<<<<<<< Updated upstream
  
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

let single_declaration_to_tree (Declaration {name; tp; body; _}) =
  PBox.tree (make_keyword_line "Declaration") 
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
    PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (DeclBlock {declarations; _}) =
PBox.tree (make_keyword_line "VarDecl")  (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
| FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
| FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]
=======

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
  (* Phase 4: comma expressions *)
  | CommaExpr {left; right; _} ->
    PBox.tree (make_info_node_line "CommaExpr") [expr_to_tree left; expr_to_tree right]
and lval_to_tree l =
  match l with
  | Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]

let single_declaration_to_tree (Declaration {name; tp; body; _}) =
  PBox.tree (make_keyword_line "Declaration")
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name];
     PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
     PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (DeclBlock {declarations; _}) =
  PBox.tree (make_keyword_line "VarDecl") (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
  | FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
  | FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]
>>>>>>> Stashed changes

let rec statement_to_tree c =
  match c with
  | VarDeclStm db -> PBox.hlist ~bars:false [PBox.line "DeclStm: "; declaration_block_to_tree db]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (make_keyword_line "IfStm")
<<<<<<< Updated upstream
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | WhileStm {cond; body; _} ->
    PBox.tree (make_keyword_line "WhileStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | ForStm {init; cond; update; body; _} ->
    PBox.tree (make_keyword_line "ForStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
        PBox.hlist ~bars:false [make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | BreakStm _ -> make_keyword_line "BreakStm"
  | ContinueStm _ -> make_keyword_line "ContinueStm"
  | CompoundStm {stms; _} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [make_keyword_line "ReturnValStm: "; expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let program_to_tree prog = 
  PBox.tree (make_info_node_line "Program") (statement_seq_to_forest prog)
=======
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with
       | None -> []
       | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | WhileStm {cond; body; _} ->
    PBox.tree (make_keyword_line "WhileStm")
      [PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
       PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | ForStm {init; cond; update; body; _} ->
    PBox.tree (make_keyword_line "ForStm")
      [PBox.hlist ~bars:false [make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
       PBox.hlist ~bars:false [make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
       PBox.hlist ~bars:false [make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
       PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | BreakStm _ -> make_keyword_line "BreakStm"
  | ContinueStm _ -> make_keyword_line "ContinueStm"
  | CompoundStm {stms; _} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  (* Phase 4: the return value is optional (return; in void functions) *)
  | ReturnStm {ret; _} ->
    PBox.hlist ~bars:false [make_keyword_line "ReturnStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

(* Phase 4: functions *)
let param_to_tree (id, tp) =
  PBox.hlist ~bars:false [ident_to_tree id; PBox.line " : "; typ_to_tree tp]

let fundecl_to_tree {ret; funname; params; body; _} =
  PBox.tree (make_keyword_line "FunDecl")
    [PBox.hlist ~bars:false [make_info_node_line "Name: "; ident_to_tree funname];
     PBox.hlist ~bars:false [make_info_node_line "Return type: "; rettyp_to_tree ret];
     PBox.tree (make_info_node_line "Params") (List.map param_to_tree params);
     PBox.tree (make_info_node_line "Body") (statement_seq_to_forest body)]

let program_fragment_to_tree (FunDecl f) = fundecl_to_tree f

let program_to_tree prog =
  PBox.tree (make_info_node_line "Program") (List.map program_fragment_to_tree prog)
>>>>>>> Stashed changes
