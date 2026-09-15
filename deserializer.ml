(* -- Use this in your solution without modifications *)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let recordname_of_yojson =
  function
  | `List [`String "RecordName"; `Assoc bnds] ->
    let name_field = string_of_yojson (List.assoc "name" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.RecordName { name = name_field ; loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed recordname JSON")

let fieldname_of_yojson =
  function
  | `List [`String "FieldName"; `Assoc bnds] ->
    let name_field = string_of_yojson (List.assoc "name" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.FieldName { name = name_field ; loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed fieldname JSON")

let ident_of_yojson =
  function
  | `List [`String "Ident"; `Assoc bnds] ->
    let name_field = string_of_yojson (List.assoc "name" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Ident { name = name_field ; loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed ident JSON")

let rec typ_of_yojson =
  function
  | `List (`String "Int"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Int { loc = loc_field }
  | `List (`String "Bool"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Bool { loc = loc_field }
  | `List (`String "Str"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Str { loc = loc_field }
  | `List (`String "Byte"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Byte { loc = loc_field }
  | `List (`String "Array"::`Assoc bnds::_) ->
    let typ_field = typ_of_yojson (List.assoc "typ" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Array { typ = typ_field ; loc = loc_field }
  | `List (`String "Record"::`Assoc bnds::_) ->
    let recordname_field = recordname_of_yojson (List.assoc "recordname" bnds) in
    Ast.Record { recordname = recordname_field }
  | _ -> failwith (Printf.sprintf "Ill-formed typ JSON")

let rettyp_of_yojson =
  function
  | `List (`String "Void"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Void { loc = loc_field }
  | `List [`String "RetTyp" ; rettyp_typ_arg] ->
    let typ_arg = typ_of_yojson rettyp_typ_arg in
    Ast.RetTyp typ_arg
  | _ -> failwith (Printf.sprintf "Ill-formed rettyp JSON")

let binop_of_yojson =
  function
  | `List (`String "Plus"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Plus {loc = loc_field}
  | `List (`String "Minus"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Minus {loc = loc_field}
  | `List (`String "Mul"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Mul {loc = loc_field}
  | `List (`String "Div"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Div {loc = loc_field}
  | `List (`String "Rem"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Rem {loc = loc_field}
  | `List (`String "Lt"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Lt {loc = loc_field}
  | `List (`String "Le"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Le {loc = loc_field}
  | `List (`String "Gt"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Gt {loc = loc_field}
  | `List (`String "Ge"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Ge {loc = loc_field}
  | `List (`String "Lor"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Lor {loc = loc_field}
  | `List (`String "Land"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Land {loc = loc_field}
  | `List (`String "Eq"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Eq {loc = loc_field}
  | `List (`String "NEq"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.NEq {loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed binop JSON")

let unop_of_yojson =
  function
  | `List (`String "Neg"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Neg {loc = loc_field}
  | `List (`String "Lnot"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Lnot {loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed unop JSON")

let rec expr_of_yojson =
  function
  | `List [`String "Integer" ; `Assoc bnds] ->
    let int_field = Int64.of_string (string_of_yojson (List.assoc "int" bnds)) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Integer { int = int_field ; loc = loc_field }
  | `List [`String "Boolean" ; `Assoc bnds] ->
    let bool_field = bool_of_yojson (List.assoc "bool" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Boolean { bool = bool_field ; loc = loc_field }
  | `List [`String "String" ; `Assoc bnds] ->
    let string_field = string_of_yojson (List.assoc "string" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.String { string = string_field ; loc = loc_field }
  | `List [`String "BinOp" ; `Assoc bnds] ->
    let left_field = expr_of_yojson (List.assoc "left" bnds) in
    let op_field = binop_of_yojson (List.assoc "op" bnds) in
    let right_field = expr_of_yojson (List.assoc "right" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.BinOp { left = left_field ; op = op_field ; right = right_field ; loc = loc_field }
  | `List [`String "UnOp" ; `Assoc bnds] ->
    let op_field = unop_of_yojson (List.assoc "op" bnds) in
    let operand_field = expr_of_yojson (List.assoc "operand" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.UnOp { op = op_field ; operand = operand_field ; loc = loc_field }
  | `List [`String "LengthOf" ; `Assoc bnds] ->
    let expr_field = expr_of_yojson (List.assoc "expr" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.LengthOf { expr = expr_field ; loc = loc_field }
  | `List [`String "Lval" ; lval_lval_arg] ->
    let lval_arg = lval_of_yojson lval_lval_arg in
    Ast.Lval lval_arg
  | `List [`String "Assignment" ; `Assoc bnds] ->
    let lvl_field = lval_of_yojson (List.assoc "lvl" bnds) in
    let rhs_field = expr_of_yojson (List.assoc "rhs" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Assignment { lvl = lvl_field ; rhs = rhs_field ; loc = loc_field }
  | `List [`String "Rcrd" ; `Assoc bnds] ->
    let rcrdtp_field = recordname_of_yojson (List.assoc "rcrdtp" bnds) in
    let fields_field =
      list_of_yojson
        (Ppx_yojson_conv_lib.Yojson_conv.pair_of_yojson fieldname_of_yojson expr_of_yojson)
        (List.assoc "fields" bnds)
    in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Rcrd { rcrdtp = rcrdtp_field ; fields = fields_field ; loc = loc_field }
  | `List [`String "Arr" ; `Assoc bnds] ->
    let tp_field = typ_of_yojson (List.assoc "tp" bnds) in
    let len_field = expr_of_yojson (List.assoc "len" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Arr { tp =  tp_field ; len = len_field ; loc = loc_field }
  | `List [`String "Nil" ; `Assoc bnds] ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Nil { loc = loc_field }
  | `List [`String "Call" ; `Assoc bnds] ->
    let fname_field = ident_of_yojson (List.assoc "fname" bnds) in
    let args_field = list_of_yojson expr_of_yojson (List.assoc "args" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Call { fname = fname_field ; args = args_field ; loc = loc_field }
  | `List [`String "CommaExpr" ; `Assoc bnds] ->
    let left_field = expr_of_yojson (List.assoc "left" bnds) in
    let right_field = expr_of_yojson (List.assoc "right" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.CommaExpr { left = left_field ; right = right_field ; loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed expr JSON")
and lval_of_yojson =
  function
  | `List [`String "Var" ; var_ident_arg] ->
    let ident_arg = ident_of_yojson var_ident_arg in
    Ast.Var ident_arg
  | `List [`String "Idx" ; `Assoc bnds] ->
    let arr_field = expr_of_yojson (List.assoc "arr" bnds) in
    let index_field = expr_of_yojson (List.assoc "index" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Idx { arr = arr_field ; index = index_field ; loc = loc_field }
  | `List [`String "Fld" ; `Assoc bnds] ->
    let rcrd_field = expr_of_yojson (List.assoc "rcrd" bnds) in
    let field_field = fieldname_of_yojson (List.assoc "field" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Fld { rcrd = rcrd_field ; field = field_field ; loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed lval JSON")

let single_declaration_of_yojson =
  function
  | `List [`String "Declaration"; `Assoc bnds] ->
    let name_field = ident_of_yojson (List.assoc "name" bnds) in
    let tp_field = option_of_yojson typ_of_yojson (List.assoc "tp" bnds) in
    let body_field = expr_of_yojson (List.assoc "body" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.Declaration {name = name_field ; tp = tp_field ; body = body_field ; loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed single_declaration JSON")

let declaration_block_of_yojson =
  function
  | `List [`String "DeclBlock"; `Assoc bnds_stm] ->
    let single_declaration_list_arg =
      list_of_yojson single_declaration_of_yojson (List.assoc "declarations" bnds_stm)
    in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds_stm) in
    Ast.DeclBlock { declarations = single_declaration_list_arg ; loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed single_declaration JSON")

let for_init_of_yojson =
  function
  | `List [`String "FIExpr"; fiexpr_expr_arg] ->
    let expr_arg =
      expr_of_yojson fiexpr_expr_arg
    in
    Ast.FIExpr expr_arg
  | `List [`String "FIDecl"; fidecl_declaration_block_arg] ->
    let declaration_block_arg =
      declaration_block_of_yojson fidecl_declaration_block_arg
    in
    Ast.FIDecl declaration_block_arg
  | _ -> failwith (Printf.sprintf "Ill-formed for_init JSON")

let rec statement_of_yojson =
  function
  | `List [`String "DeclStm"; vardeclstm_declaration_block_arg ] ->
    let declaration_block_arg = declaration_block_of_yojson vardeclstm_declaration_block_arg in
    Ast.VarDeclStm declaration_block_arg
  | `List [`String "ExprStm"; `Assoc bnds] ->
    let expr_field = option_of_yojson expr_of_yojson (List.assoc "expr" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.ExprStm { expr = expr_field ; loc = loc_field}
  | `List [`String "IfThenElseStm"; `Assoc bnds] ->
    let cond_field = expr_of_yojson (List.assoc "cond" bnds) in
    let thbr_field = statement_of_yojson (List.assoc "thbr" bnds) in
    let elbro_field = option_of_yojson statement_of_yojson (List.assoc "elbro" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.IfThenElseStm {cond = cond_field ; thbr = thbr_field ; elbro = elbro_field ; loc = loc_field}
  | `List [`String "WhileStm"; `Assoc bnds] ->
    let cond_field = expr_of_yojson (List.assoc "cond" bnds) in
    let body_field = statement_of_yojson (List.assoc "body" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.WhileStm {cond = cond_field ; body = body_field ; loc = loc_field}
  | `List [`String "ForStm"; `Assoc bnds] ->
    let init_field = option_of_yojson for_init_of_yojson (List.assoc "init" bnds) in
    let cond_field = option_of_yojson expr_of_yojson (List.assoc "cond" bnds) in
    let update_field = option_of_yojson expr_of_yojson (List.assoc "update" bnds) in
    let body_field = statement_of_yojson (List.assoc "body" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.ForStm {init = init_field ; cond = cond_field ; update = update_field ; body = body_field ; loc = loc_field}
  | `List (`String "BreakStm"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.BreakStm { loc = loc_field }
  | `List (`String "ContinueStm"::`Assoc bnds::_) ->
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.ContinueStm { loc = loc_field }
  | `List [`String "CompoundStm"; `Assoc bnds] ->
    let stms_field = list_of_yojson statement_of_yojson (List.assoc "stms" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.CompoundStm { stms = stms_field ; loc = loc_field}
  | `List [`String "ReturnStm"; `Assoc bnds] ->
    let ret_field = option_of_yojson expr_of_yojson (List.assoc "ret" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    Ast.ReturnStm { ret = ret_field ; loc = loc_field}
  | _ -> failwith (Printf.sprintf "Ill-formed statement JSON")


let fundecl_of_yojson =
  function
  | `Assoc bnds ->
    let ret_field = rettyp_of_yojson (List.assoc "ret" bnds) in
    let funname_field = ident_of_yojson (List.assoc "funname" bnds) in
    let params_field =
      list_of_yojson
        (Ppx_yojson_conv_lib.Yojson_conv.pair_of_yojson ident_of_yojson typ_of_yojson)
        (List.assoc "params" bnds)
    in
    let body_field = list_of_yojson statement_of_yojson (List.assoc "body" bnds) in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    { Ast.ret = ret_field ; Ast.funname = funname_field ; Ast.params = params_field ; Ast.body = body_field ; Ast.loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed fundecl JSON")

let recdecl_of_yojson =
  function
  | `Assoc bnds ->
    let recname_field = recordname_of_yojson (List.assoc "recname" bnds) in
    let fields_field =
      list_of_yojson
        (Ppx_yojson_conv_lib.Yojson_conv.pair_of_yojson fieldname_of_yojson typ_of_yojson)
        (List.assoc "fields" bnds)
    in
    let loc_field = Location.location_of_yojson (List.assoc "loc" bnds) in
    { Ast.recname = recname_field ; Ast.fields = fields_field ; Ast.loc = loc_field }
  | _ -> failwith (Printf.sprintf "Ill-formed recdecl JSON")

let program_fragment_of_yojson =
  function
  | `List [`String "FunDecl"; fundecl_fundecl_arg] ->
    let fundecl_arg = fundecl_of_yojson fundecl_fundecl_arg in
    Ast.FunDecl fundecl_arg
  | `List [`String "RecDecl"; recdecl_recdecl_arg] ->
    let recdecl_arg = recdecl_of_yojson recdecl_recdecl_arg in
    Ast.RecDecl recdecl_arg
  | _ -> failwith (Printf.sprintf "Ill-formed program_fragment JSON")

let program_of_yojson =
  fun t -> list_of_yojson program_fragment_of_yojson t
