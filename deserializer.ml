(* -- Use this in your solution without modifications *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
let ident_of_yojson =
  function
  | `List [`String "Ident"; `Assoc bnds] ->
    let name_field = string_of_yojson (List.assoc "name" bnds) in
    Ast.Ident { name = name_field}
  | _ -> failwith (Printf.sprintf "Ill-formed ident JSON")

let typ_of_yojson =
  function
  | `List (`String "Int"::_) -> Ast.Int
  | `List (`String "Bool"::_) -> Ast.Bool
  | _ -> failwith (Printf.sprintf "Ill-formed typ JSON")

let binop_of_yojson =
  function
  | `List (`String "Plus":: _ ) -> Ast.Plus
  | `List (`String "Minus":: _) -> Ast.Minus
  | `List (`String "Mul":: _) -> Ast.Mul
  | `List (`String "Div":: _) -> Ast.Div
  | `List (`String "Rem":: _) -> Ast.Rem
  | `List (`String "Lt":: _) -> Ast.Lt
  | `List (`String "Le":: _) -> Ast.Le
  | `List (`String "Gt":: _) -> Ast.Gt
  | `List (`String "Ge":: _) -> Ast.Ge
  | `List (`String "Lor":: _) -> Ast.Lor
  | `List (`String "Land":: _) -> Ast.Land
  | `List (`String "Eq":: _) -> Ast.Eq
  | `List (`String "NEq":: _) -> Ast.NEq
  | _ -> failwith (Printf.sprintf "Ill-formed binop JSON")

let unop_of_yojson =
  function
  | `List (`String "Neg":: _ ) -> Ast.Neg
  | `List (`String "Lnot":: _ ) -> Ast.Lnot
  | _ -> failwith (Printf.sprintf "Ill-formed unop JSON")

let rec expr_of_yojson =
  function
  | `List [`String "Integer" ; `Assoc bnds] ->
    let int_field = Int64.of_string (string_of_yojson (List.assoc "int" bnds)) in
    Ast.Integer { int = int_field }
  | `List [`String "Boolean" ; `Assoc bnds] ->
    let bool_field = bool_of_yojson (List.assoc "bool" bnds) in
    Ast.Boolean { bool = bool_field }
  | `List [`String "BinOp" ; `Assoc bnds] ->
    let left_field = expr_of_yojson (List.assoc "left" bnds) in
    let op_field = binop_of_yojson (List.assoc "op" bnds) in
    let right_field = expr_of_yojson (List.assoc "right" bnds) in
    Ast.BinOp { left = left_field ; op = op_field ; right = right_field }
  | `List [`String "UnOp" ; `Assoc bnds] ->
    let op_field = unop_of_yojson (List.assoc "op" bnds) in
    let operand_field = expr_of_yojson (List.assoc "operand" bnds) in
    Ast.UnOp { op = op_field ; operand = operand_field }
  | `List [`String "Lval" ; lval_lval_arg] ->
    let lval_arg = lval_of_yojson lval_lval_arg in
    Ast.Lval lval_arg
  | `List [`String "Assignment" ; `Assoc bnds] ->
    let lvl_field = lval_of_yojson (List.assoc "lvl" bnds) in
    let rhs_field = expr_of_yojson (List.assoc "rhs" bnds) in
    Ast.Assignment { lvl = lvl_field ; rhs = rhs_field }
  | `List [`String "Call" ; `Assoc bnds] ->
    let fname_field = ident_of_yojson (List.assoc "fname" bnds) in
    let args_field = list_of_yojson expr_of_yojson (List.assoc "args" bnds) in
    Ast.Call { fname = fname_field ; args = args_field }
  | _ -> failwith (Printf.sprintf "Ill-formed expr JSON")
and lval_of_yojson =
  function
  | `List [`String "Var" ; var_ident_arg] ->
    let ident_arg = ident_of_yojson var_ident_arg in
    Ast.Var ident_arg
  | _ -> failwith (Printf.sprintf "Ill-formed lval JSON")

let rec statement_of_yojson =
  function
  | `List [`String "DeclStm"; `List [`String "DeclBlock"; `Assoc bnds_stm]] ->
    begin
      match List.assoc "declarations" bnds_stm with
      | `List [`List [`String "Declaration"; `Assoc bnds] ] ->
        let name_field = ident_of_yojson (List.assoc "name" bnds) in
        let tp_field = option_of_yojson typ_of_yojson (List.assoc "tp" bnds) in
        let body_field = expr_of_yojson (List.assoc "body" bnds) in
        Ast.VarDeclStm {name = name_field ; tp = tp_field ; body = body_field}
      | _ -> failwith (Printf.sprintf "Ill-formed statement declaration JSON")
    end
  | `List [`String "ExprStm"; `Assoc bnds] ->
    let expr_field = option_of_yojson expr_of_yojson (List.assoc "expr" bnds) in
    Ast.ExprStm { expr = expr_field}
  | `List [`String "IfThenElseStm"; `Assoc bnds] ->
    let cond_field = expr_of_yojson (List.assoc "cond" bnds) in
    let thbr_field = statement_of_yojson (List.assoc "thbr" bnds) in
    let elbro_field = option_of_yojson statement_of_yojson (List.assoc "elbro" bnds) in
    Ast.IfThenElseStm {cond = cond_field ; thbr = thbr_field ; elbro = elbro_field}
  | `List [`String "CompoundStm"; `Assoc bnds] ->
    let stms_field = list_of_yojson statement_of_yojson (List.assoc "stms" bnds) in
    Ast.CompoundStm { stms = stms_field}
  | `List [`String "ReturnStm"; `Assoc bnds] ->
    let ret_field = expr_of_yojson (List.assoc "ret" bnds) in
    ReturnStm { ret = ret_field}
  | _ -> failwith (Printf.sprintf "Ill-formed statement JSON")

let program_of_yojson =
  fun t -> list_of_yojson statement_of_yojson t