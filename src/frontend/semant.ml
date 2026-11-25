(* Semantic analysis for Dolphin, phase 5.

   Error handling: instead of stopping at the first error, every error is
   recorded in [errors] and the analysis continues. An expression with an
   error gets the type [ErrorType], which acts as a wildcard: it never causes
   further errors, so one mistake does not produce a cascade of follow-up
   errors. At the end, if any errors were recorded, [TypeError] is raised with
   all of them, in source order. *)

exception TypeError of string list

module TAst = TypedAst
module Loc = Location

(* ------------------------------------------------------------------------
   Error collection
   ------------------------------------------------------------------------ *)

let errors : Errors.error list ref = ref []        (* newest first *)
let report err = errors := err :: !errors

(* ------------------------------------------------------------------------
   Helpers
   ------------------------------------------------------------------------ *)

let name_of (Ast.Ident { name; _ }) = name
let recordname_of (Ast.RecordName { name; _ }) = name
let fieldname_of (Ast.FieldName { name; _ }) = name

let tident (Ast.Ident { name; _ }) = TAst.Ident { sym = Symbol.symbol name }
let trecordname (Ast.RecordName { name; _ }) = TAst.RecordName { sym = Symbol.symbol name }
let tfieldname (Ast.FieldName { name; _ }) = TAst.FieldName { sym = Symbol.symbol name }

let loc_of_ident (Ast.Ident { loc; _ }) = loc
let loc_of_recordname (Ast.RecordName { loc; _ }) = loc
let loc_of_fieldname (Ast.FieldName { loc; _ }) = loc

let loc_of_lval = function
  | Ast.Var id -> loc_of_ident id
  | Ast.Idx { loc; _ } | Ast.Fld { loc; _ } -> loc

let loc_of_expr = function
  | Ast.Integer { loc; _ } | Ast.Boolean { loc; _ } | Ast.String { loc; _ }
  | Ast.BinOp { loc; _ } | Ast.UnOp { loc; _ } | Ast.LengthOf { loc; _ }
  | Ast.Assignment { loc; _ } | Ast.Rcrd { loc; _ } | Ast.Arr { loc; _ }
  | Ast.Nil { loc } | Ast.CommaExpr { loc; _ } | Ast.Call { loc; _ } -> loc
  | Ast.Lval lv -> loc_of_lval lv

let rec resolve_typ rec_exists = function
  | Ast.Int _ -> TAst.Int
  | Ast.Bool _ -> TAst.Bool
  | Ast.Str _ -> TAst.Str
  | Ast.Byte _ -> TAst.Byte
  | Ast.Array { typ; _ } -> TAst.Array (resolve_typ rec_exists typ)
  | Ast.Record { recordname = Ast.RecordName { name; loc } as rn } ->
    if rec_exists name then TAst.Record (trecordname rn)
    else begin
      report (Errors.UnknownRecordType { name; loc });
      TAst.ErrorType
    end

let typecheck_typ env tp =
  resolve_typ (fun name -> Env.lookup_record env name <> None) tp

let typecheck_binop = function
  | Ast.Plus _ -> TAst.Plus | Ast.Minus _ -> TAst.Minus | Ast.Mul _ -> TAst.Mul
  | Ast.Div _  -> TAst.Div  | Ast.Rem _   -> TAst.Rem   | Ast.Lt _  -> TAst.Lt
  | Ast.Le _   -> TAst.Le   | Ast.Gt _    -> TAst.Gt    | Ast.Ge _  -> TAst.Ge
  | Ast.Lor _  -> TAst.Lor  | Ast.Land _  -> TAst.Land  | Ast.Eq _  -> TAst.Eq
  | Ast.NEq _  -> TAst.NEq

let binop_to_string = function
  | Ast.Plus _ -> "+"  | Ast.Minus _ -> "-"  | Ast.Mul _ -> "*"  | Ast.Div _ -> "/"
  | Ast.Rem _  -> "%"  | Ast.Lt _    -> "<"  | Ast.Le _  -> "<=" | Ast.Gt _  -> ">"
  | Ast.Ge _   -> ">=" | Ast.Lor _   -> "||" | Ast.Land _ -> "&&" | Ast.Eq _ -> "=="
  | Ast.NEq _  -> "!="

let unop_to_string = function
  | Ast.Neg _  -> "-"
  | Ast.Lnot _ -> "!"

(* Env signals an unknown name with an exception; turn that into an option. *)
let lookup env id =
  match Env.lookup_var_fun env id with
  | entry -> Some entry
  | exception (Failure _ | Not_found) -> None

let is_error tp = tp = TAst.ErrorType

let is_nullable = function
  | TAst.Record _ | TAst.Array _ -> true
  | _ -> false

(* Check whether a value of type [actual] can be used where [expected] is required. *)
let is_compatible ~expected ~actual =
  is_error expected || is_error actual || expected = actual
  || (actual = TAst.NilType && is_nullable expected)

(* ------------------------------------------------------------------------
   Expressions & L-values
   ------------------------------------------------------------------------ *)

(* Returns the typed expression and its type. Never raises: errors are
   reported and the expression gets type [ErrorType]. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer { int; _ } -> (TAst.Integer { int }, TAst.Int)
  | Ast.Boolean { bool; _ } -> (TAst.Boolean { bool }, TAst.Bool)
  | Ast.String { string; _ } -> (TAst.String { string }, TAst.Str)
  | Ast.Nil _ -> (TAst.Nil, TAst.NilType)
  | Ast.BinOp { left; op; right; loc } ->
    let tleft, lt = infertype_expr env left in
    let tright, rt = infertype_expr env right in
    (* If an operand already has an error, don't report a follow-up error. *)
    let check ok =
      if not (is_error lt || is_error rt) && not ok then
        report (Errors.BinOpTypeError { op = binop_to_string op; left = lt; right = rt; loc })
    in
    (* The result type is determined by the operator alone, so an error in an
       operand does not spread to the enclosing expression. *)
    let tp =
      match op with
      | Ast.Plus _ | Ast.Minus _ | Ast.Mul _ | Ast.Div _ | Ast.Rem _ ->
        check (lt = TAst.Int && rt = TAst.Int); TAst.Int
      | Ast.Lt _ | Ast.Le _ | Ast.Gt _ | Ast.Ge _ ->
        check ((lt = TAst.Int && rt = TAst.Int) || (lt = TAst.Str && rt = TAst.Str));
        TAst.Bool
      | Ast.Eq _ | Ast.NEq _ ->
        let eq_ok =
          lt = rt
          || (is_nullable lt && rt = TAst.NilType)
          || (lt = TAst.NilType && is_nullable rt)
        in
        check eq_ok; TAst.Bool
      | Ast.Land _ | Ast.Lor _ ->
        check (lt = TAst.Bool && rt = TAst.Bool); TAst.Bool
    in
    (TAst.BinOp { left = tleft; op = typecheck_binop op; right = tright; tp }, tp)
  | Ast.UnOp { op; operand; loc } ->
    let toperand, ot = infertype_expr env operand in
    let expected, top =
      match op with
      | Ast.Neg _  -> (TAst.Int, TAst.Neg)
      | Ast.Lnot _ -> (TAst.Bool, TAst.Lnot)
    in
    if not (is_error ot) && ot <> expected then
      report (Errors.UnOpTypeError { op = unop_to_string op; operand = ot; loc });
    (TAst.UnOp { op = top; operand = toperand; tp = expected }, expected)
  | Ast.LengthOf { expr = inner; loc } ->
    let tinner, inner_tp = infertype_expr env inner in
    if not (is_error inner_tp) then begin
      match inner_tp with
      | TAst.Str | TAst.Array _ -> ()
      | _ -> report (Errors.LengthOfInvalidType { actual = inner_tp; loc })
    end;
    (TAst.LengthOf { expr = tinner }, TAst.Int)
  | Ast.Lval lval ->
    let tlval, tp = infertype_lval env lval in
    (TAst.Lval tlval, tp)
  | Ast.Assignment { lvl; rhs; loc } ->
    let tlvl, lt = infertype_lval env lvl in
    let trhs, rt = infertype_expr env rhs in
    if not (is_compatible ~expected:lt ~actual:rt) then
      report (Errors.TypeMismatch { context = "assignment"; expected = lt; actual = rt; loc });
    (TAst.Assignment { lvl = tlvl; rhs = trhs; tp = lt }, lt)
  | Ast.Rcrd { rcrdtp; fields; loc } ->
    let rname = recordname_of rcrdtp in
    let trname = trecordname rcrdtp in
    (match Env.lookup_record env rname with
     | None ->
       report (Errors.UnknownRecordType { name = rname; loc = loc_of_recordname rcrdtp });
       let tfields =
         List.map (fun (f, e) -> let te, _ = infertype_expr env e in (tfieldname f, te)) fields
       in
       (TAst.Rcrd { recname = trname; fields = tfields }, TAst.ErrorType)
     | Some info ->
       if info.Env.reserved then
         report (Errors.ReservedRecordCreation { name = rname; loc });
       let seen_inits = Hashtbl.create (List.length fields) in
       let tfields =
         List.map
           (fun (fname, fexpr) ->
              let fn = fieldname_of fname in
              let floc = loc_of_fieldname fname in
              let is_dup = Hashtbl.mem seen_inits fn in
              if is_dup then
                report (Errors.DuplicateFieldInit { recname = rname; field = fn; loc = floc })
              else
                Hashtbl.add seen_inits fn ();
              let te =
                match Env.lookup_field info fn with
                | Some expected ->
                  typecheck_expr env fexpr expected
                    ("field '" ^ fn ^ "' of record '" ^ rname ^ "'")
                | None ->
                  if not is_dup then
                    report (Errors.UnknownField { recname = rname; field = fn; loc = floc });
                  fst (infertype_expr env fexpr)
              in
              (tfieldname fname, te))
           fields
       in
       List.iter
         (fun (fn, _) ->
            if not (Hashtbl.mem seen_inits fn) then
              report (Errors.MissingFieldInit { recname = rname; field = fn; loc }))
         info.Env.fields;
       (TAst.Rcrd { recname = trname; fields = tfields }, TAst.Record trname))
  | Ast.Arr { tp; len; _ } ->
    let elemtp = typecheck_typ env tp in
    let tlen = typecheck_expr env len TAst.Int "array length" in
    let arr_tp = if is_error elemtp then TAst.ErrorType else TAst.Array elemtp in
    (TAst.Arr { elemtp; len = tlen }, arr_tp)
  | Ast.Call { fname; args; loc } ->
    let tcall, ret = typecheck_call env fname args loc in
    (match ret with
     | TAst.RetTyp tp -> (tcall, tp)
     | TAst.Void ->
       report (Errors.VoidAsValue { name = name_of fname; loc });
       (tcall, TAst.ErrorType))
  | Ast.CommaExpr { left; right; _ } ->
    let tleft, _ = infertype_expr env left in
    let tright, rt = infertype_expr env right in
    (TAst.CommaExpr { left = tleft; right = tright; tp = rt }, rt)

and infertype_lval env = function
  | Ast.Var id ->
    let tp =
      match lookup env id with
      | Some (Env.Var tp) -> tp
      | Some (Env.Fun _) ->
        report (Errors.FunctionAsValue { name = name_of id; loc = loc_of_ident id });
        TAst.ErrorType
      | None ->
        report (Errors.UnboundIdentifier { name = name_of id; loc = loc_of_ident id });
        TAst.ErrorType
    in
    (TAst.Var { ident = tident id; tp }, tp)
  | Ast.Idx { arr; index; loc } ->
    let tarr, arr_tp = infertype_expr env arr in
    let tindex = typecheck_expr env index TAst.Int "array index" in
    let elem_tp =
      match arr_tp with
      | TAst.Array tp -> tp
      | TAst.ErrorType -> TAst.ErrorType
      | _ ->
        report (Errors.IndexOnNonArray { actual = arr_tp; loc });
        TAst.ErrorType
    in
    (TAst.Idx { arr = tarr; index = tindex; tp = elem_tp }, elem_tp)
  | Ast.Fld { rcrd; field; loc } ->
    let trcrd, rcrd_tp = infertype_expr env rcrd in
    let fn = fieldname_of field in
    let dummy_rec = TAst.RecordName { sym = Symbol.symbol "<error>" } in
    let recname, tp =
      match rcrd_tp with
      | TAst.Record (TAst.RecordName { sym } as rn) ->
        (match Env.lookup_record env (Symbol.name sym) with
         | Some info ->
           (match Env.lookup_field info fn with
            | Some ftp -> (rn, ftp)
            | None ->
              report (Errors.UnknownField { recname = Symbol.name sym; field = fn; loc = loc_of_fieldname field });
              (rn, TAst.ErrorType))
         | None -> (rn, TAst.ErrorType))
      | TAst.ErrorType -> (dummy_rec, TAst.ErrorType)
      | _ ->
        report (Errors.FieldAccessOnNonRecord { actual = rcrd_tp; loc });
        (dummy_rec, TAst.ErrorType)
    in
    (TAst.Fld { rcrd = trcrd; recname; field = tfieldname field; tp }, tp)

(* Shared by calls used as values and calls used as statements. The arguments
   are always checked, even if the function is unknown, so errors inside them
   are reported too. *)
and typecheck_call env fname args loc =
  let targs = List.map (infertype_expr env) args in
  let typed_args = List.map fst targs in
  let arg_tps = List.map snd targs in
  let name = name_of fname in
  let failed () =
    let tp = TAst.RetTyp TAst.ErrorType in
    (TAst.Call { fname = tident fname; args = typed_args; tp }, tp)
  in
  match lookup env fname with
  | None ->
    report (Errors.UnboundIdentifier { name; loc = loc_of_ident fname }); failed ()
  | Some (Env.Var _) ->
    report (Errors.NotAFunction { name; loc = loc_of_ident fname }); failed ()
  | Some (Env.Fun (TAst.FunTyp { ret; params })) ->
    let param_tps = List.map (fun (TAst.Param { typ; _ }) -> typ) params in
    if List.length param_tps <> List.length arg_tps then
      report (Errors.ArgumentCountMismatch
                { name; expected = List.length param_tps; actual = List.length arg_tps; loc })
    else
      List.iteri
        (fun i ((expected, actual), arg) ->
           if not (is_compatible ~expected ~actual) then
             report (Errors.ArgumentTypeMismatch
                       { name; index = i + 1; expected; actual; loc = loc_of_expr arg }))
        (List.combine (List.combine param_tps arg_tps) args);
    (TAst.Call { fname = tident fname; args = typed_args; tp = ret }, ret)

(* Checks that [expr] has type compatible with [expected]; [context] is used in the message. *)
and typecheck_expr env expr expected context =
  let texpr, actual = infertype_expr env expr in
  if not (is_compatible ~expected ~actual) then
    report (Errors.TypeMismatch { context; expected; actual; loc = loc_of_expr expr });
  texpr

(* An expression whose value is discarded: void calls are allowed here. *)
and infertype_expr_ret env expr =
  match expr with
  | Ast.Call { fname; args; loc } -> typecheck_call env fname args loc
  | Ast.CommaExpr { left; right; _ } ->
    let tleft, _ = infertype_expr_ret env left in
    let tright, rt = infertype_expr_ret env right in
    let tp = match rt with TAst.RetTyp t -> t | TAst.Void -> TAst.ErrorType in
    (TAst.CommaExpr { left = tleft; right = tright; tp }, rt)
  | _ ->
    let te, tp = infertype_expr env expr in
    (te, TAst.RetTyp tp)

and typecheck_discarded_expr env expr = fst (infertype_expr_ret env expr)

(* ------------------------------------------------------------------------
   Declarations
   ------------------------------------------------------------------------ *)

(* The initializer is checked before the variable is added to the
   environment. A variable whose type could not be determined gets type
   [ErrorType], so later uses of it do not produce follow-up errors. *)
let typecheck_single_declaration env (Ast.Declaration { name; tp; body; _ }) =
  let tbody, body_tp = infertype_expr env body in
  let result_tp =
    match tp with
    | Some t ->
      let expected = typecheck_typ env t in
      if not (is_compatible ~expected ~actual:body_tp) then
        report (Errors.TypeMismatch
                  { context = "declaration of '" ^ name_of name ^ "'";
                    expected; actual = body_tp; loc = loc_of_expr body });
      expected
    | None ->
      if body_tp = TAst.NilType then begin
        report (Errors.UninferableNil { loc = loc_of_expr body });
        TAst.ErrorType
      end else
        body_tp
  in
  let new_env = Env.insert_local_decl env name result_tp in
  (TAst.Declaration { name = tident name; tp = result_tp; body = tbody }, new_env)

(* Left to right: each variable is visible in the following initializers. *)
let typecheck_declaration_block env (Ast.DeclBlock { declarations; _ }) =
  let final_env, typed_declarations =
    List.fold_left_map
      (fun current_env declaration ->
         let typed_decl, next_env = typecheck_single_declaration current_env declaration in
         (next_env, typed_decl))
      env declarations
  in
  (TAst.DeclBlock typed_declarations, final_env)

let typecheck_for_init env = function
  | Ast.FIExpr expr -> (TAst.FIExpr (typecheck_discarded_expr env expr), env)
  | Ast.FIDecl decl_block ->
    let typed_block, new_env = typecheck_declaration_block env decl_block in
    (TAst.FIDecl typed_block, new_env)

(* ------------------------------------------------------------------------
   Statements
   ------------------------------------------------------------------------ *)

(* Returns (typed statement, environment for the following statements). *)
let rec typecheck_statement env ret_type stm =
  match stm with
  | Ast.VarDeclStm decl_block ->
    let typed_block, new_env = typecheck_declaration_block env decl_block in
    (TAst.VarDeclStm typed_block, new_env)

  | Ast.ExprStm { expr; _ } ->
    let texpr =
      match expr with
      | None -> None
      | Some (Ast.Call _ as e) -> Some (typecheck_discarded_expr env e)
      | Some (Ast.Assignment _ as e) -> Some (fst (infertype_expr env e))
      | Some e ->
        report (Errors.InvalidExpressionStatement { loc = loc_of_expr e });
        Some (typecheck_discarded_expr env e)
    in
    (TAst.ExprStm { expr = texpr }, env)

  | Ast.IfThenElseStm { cond; thbr; elbro; _ } ->
    let tcond = typecheck_expr env cond TAst.Bool "if condition" in
    let tthbr, _ = typecheck_statement env ret_type thbr in
    let telbro = Option.map (fun s -> fst (typecheck_statement env ret_type s)) elbro in
    (TAst.IfThenElseStm { cond = tcond; thbr = tthbr; elbro = telbro }, env)

  | Ast.CompoundStm { stms; _ } ->
    let tstms, _ = typecheck_statement_seq env ret_type stms in
    (TAst.CompoundStm { stms = tstms }, env)

  | Ast.ReturnStm { ret; loc } ->
    let tret =
      match ret, ret_type with
      | Some e, TAst.RetTyp t -> Some (typecheck_expr env e t "return value")
      | Some e, TAst.Void ->
        report (Errors.ReturnValInVoid { loc });
        Some (fst (infertype_expr env e))
      | None, TAst.Void -> None
      | None, TAst.RetTyp _ ->
        report (Errors.ReturnWithoutVal { loc });
        None
    in
    (TAst.ReturnStm { ret = tret }, env)

  | Ast.WhileStm { cond; body; _ } ->
    let tcond = typecheck_expr env cond TAst.Bool "while condition" in
    let tbody, _ = typecheck_statement (Env.enter_loop env) ret_type body in
    (TAst.WhileStm { cond = tcond; body = tbody }, env)

  | Ast.ForStm { init; cond; update; body; _ } ->
    (* Declarations in init are visible in cond, update and body only. *)
    let typed_init, for_env =
      match init with
      | None -> (None, env)
      | Some fi -> let tfi, e = typecheck_for_init env fi in (Some tfi, e)
    in
    let typed_cond =
      Option.map (fun c -> typecheck_expr for_env c TAst.Bool "for condition") cond
    in
    let typed_update = Option.map (typecheck_discarded_expr for_env) update in
    let typed_body, _ = typecheck_statement (Env.enter_loop for_env) ret_type body in
    (TAst.ForStm { init = typed_init; cond = typed_cond;
                   update = typed_update; body = typed_body }, env)

  | Ast.BreakStm { loc } ->
    if not (Env.is_inside_loop env) then report (Errors.BreakOutsideLoop { loc });
    (TAst.BreakStm, env)

  | Ast.ContinueStm { loc } ->
    if not (Env.is_inside_loop env) then report (Errors.ContinueOutsideLoop { loc });
    (TAst.ContinueStm, env)

and typecheck_statement_seq env ret_tp stms =
  let result_env, tstms =
    List.fold_left_map
      (fun env s -> let ts, env' = typecheck_statement env ret_tp s in (env', ts))
      env stms
  in
  (tstms, result_env)

(* ------------------------------------------------------------------------
   Records & Functions
   ------------------------------------------------------------------------ *)

let reserved_record_names = [
  "stream";
  "socket";
  "socket_address";
  "ip_version";
  "accepted_connection";
  "udp_recvfrom_result";
  "connection_type";
]

let reserved_record_entries =
  List.map (fun name -> (name, { Env.fields = []; reserved = true })) reserved_record_names

let typecheck_records (recs : Ast.recdecl list) =
  (* Pass 1: collect all record names and check duplicates / reserved collisions. *)
  let known = Hashtbl.create (List.length recs + 8) in
  List.iter (fun name -> Hashtbl.add known name ()) reserved_record_names;
  let unique_recs =
    List.filter
      (fun (r : Ast.recdecl) ->
         let rname = recordname_of r.Ast.recname in
         let loc = loc_of_recordname r.Ast.recname in
         if List.mem rname reserved_record_names then begin
           report (Errors.ReservedRecordName { name = rname; loc });
           false
         end else if Hashtbl.mem known rname then begin
           report (Errors.DuplicateRecord { name = rname; loc });
           false
         end else begin
           Hashtbl.add known rname ();
           true
         end)
      recs
  in
  (* Pass 2: validate field uniqueness and resolve field types (mutually recursive). *)
  let rec_exists name = Hashtbl.mem known name in
  let check_rec (r : Ast.recdecl) =
    let rname = recordname_of r.Ast.recname in
    let seen_fields = Hashtbl.create (List.length r.Ast.fields) in
    let env_fields_rev, tfields =
      List.fold_left_map
        (fun acc (fname, ftyp) ->
           let fn = fieldname_of fname in
           let floc = loc_of_fieldname fname in
           let tp = resolve_typ rec_exists ftyp in
           let acc' =
             if Hashtbl.mem seen_fields fn then begin
               report (Errors.DuplicateField { recname = rname; field = fn; loc = floc });
               acc
             end else begin
               Hashtbl.add seen_fields fn ();
               (fn, tp) :: acc
             end
           in
           (acc', (tfieldname fname, tp)))
        [] r.Ast.fields
    in
    let typed_rec =
      { TAst.recname = trecordname r.Ast.recname; fields = tfields }
    in
    let env_entry =
      (rname, { Env.fields = List.rev env_fields_rev; reserved = false })
    in
    (env_entry, typed_rec)
  in
  let user_entries, typed_recs = List.split (List.map check_rec unique_recs) in
  (reserved_record_entries @ user_entries, typed_recs)

let rec returns = function
  | Ast.ReturnStm _ -> true
  | Ast.IfThenElseStm { thbr; elbro = Some el; _ } -> returns thbr && returns el
  | Ast.CompoundStm { stms; _ } -> List.exists returns stms
  | _ -> false

let typecheck_rettyp env = function
  | Ast.Void _ -> TAst.Void
  | Ast.RetTyp t -> TAst.RetTyp (typecheck_typ env t)

let make_builtin name param_tys ret =
  let params =
    List.mapi
      (fun i typ ->
         TAst.Param { paramname = TAst.Ident { sym = Symbol.symbol ("arg" ^ string_of_int i) }; typ })
      param_tys
  in
  (name, TAst.FunTyp { ret; params })

let stream_t = TAst.Record (TAst.RecordName { sym = Symbol.symbol "stream" })

let initial_environment = [
  (* Phase 5 core standard library *)
  make_builtin "bytes_array_to_string" [TAst.Array TAst.Byte] (TAst.RetTyp TAst.Str);
  make_builtin "string_to_bytes_array" [TAst.Str] (TAst.RetTyp (TAst.Array TAst.Byte));
  make_builtin "byte_to_int_unsigned"  [TAst.Byte] (TAst.RetTyp TAst.Int);
  make_builtin "byte_to_int_signed"    [TAst.Byte] (TAst.RetTyp TAst.Int);
  make_builtin "int_to_byte_unsigned"  [TAst.Int] (TAst.RetTyp TAst.Byte);
  make_builtin "int_to_byte_signed"    [TAst.Int] (TAst.RetTyp TAst.Byte);
  make_builtin "ascii_ord"             [TAst.Str] (TAst.RetTyp TAst.Int);
  make_builtin "ascii_chr"             [TAst.Int] (TAst.RetTyp TAst.Str);
  make_builtin "string_concat"         [TAst.Str; TAst.Str] (TAst.RetTyp TAst.Str);
  make_builtin "substring"             [TAst.Str; TAst.Int; TAst.Int] (TAst.RetTyp TAst.Str);
  make_builtin "int_to_string"         [TAst.Int] (TAst.RetTyp TAst.Str);
  make_builtin "string_to_int"         [TAst.Str] (TAst.RetTyp TAst.Int);
  make_builtin "input_byte"            [stream_t] (TAst.RetTyp TAst.Int);
  make_builtin "output_byte"           [TAst.Byte; stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "input_bytes_array"     [TAst.Int; stream_t] (TAst.RetTyp (TAst.Array TAst.Byte));
  make_builtin "output_bytes_array"    [TAst.Array TAst.Byte; stream_t] TAst.Void;
  make_builtin "output_string"         [TAst.Str; stream_t] TAst.Void;
  make_builtin "seek_in_file"          [TAst.Int; TAst.Bool; stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "pos_in_file"           [stream_t] (TAst.RetTyp TAst.Int);
  make_builtin "close_file"            [stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "flush_file"            [stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "error_in_file"         [stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "end_of_file"           [stream_t] (TAst.RetTyp TAst.Bool);
  make_builtin "get_eof"               [] (TAst.RetTyp TAst.Int);
  make_builtin "open_file"             [TAst.Str; TAst.Str] (TAst.RetTyp stream_t);
  make_builtin "get_stdin"             [] (TAst.RetTyp stream_t);
  make_builtin "get_stderr"            [] (TAst.RetTyp stream_t);
  make_builtin "get_stdout"            [] (TAst.RetTyp stream_t);
  make_builtin "get_cmd_args"          [] (TAst.RetTyp (TAst.Array TAst.Str));
  make_builtin "exit"                  [TAst.Int] TAst.Void;
  (* Kept from earlier phases (still in stdlib.c) *)
  make_builtin "print_integer"         [TAst.Int] TAst.Void;
  make_builtin "read_integer"          [] (TAst.RetTyp TAst.Int);
]

let funtype_of env (f : Ast.fundecl) =
  TAst.FunTyp {
    ret = typecheck_rettyp env f.Ast.ret;
    params = List.map (fun (id, t) ->
      TAst.Param { paramname = tident id; typ = typecheck_typ env t }) f.Ast.params;
  }

let typecheck_fundecl env (f : Ast.fundecl) (TAst.FunTyp { ret = ret_tp; params = tparams }) =
  let seen = Hashtbl.create 8 in
  let body_env =
    List.fold_left2
      (fun env (id, _) (TAst.Param { typ = tp; _ }) ->
         let name = name_of id in
         if Hashtbl.mem seen name then begin
           report (Errors.DuplicateParam { name; loc = loc_of_ident id });
           env
         end else begin
           Hashtbl.add seen name ();
           Env.insert_local_decl env id tp
         end)
      env f.Ast.params tparams
  in
  let tbody, _ = typecheck_statement_seq body_env ret_tp f.Ast.body in
  if ret_tp <> TAst.Void && not (List.exists returns f.Ast.body) then begin
    let loc = if f.Ast.body = [] then None else Some f.Ast.loc in
    report (Errors.MissingReturn { loc })
  end;
  TAst.FunDecl {
    funname = tident f.Ast.funname;
    ret = ret_tp;
    params = tparams;
    body = tbody;
  }

(* ------------------------------------------------------------------------
   Checking the whole program (records and functions)
   ------------------------------------------------------------------------ *)

let typecheck_prog prg =
  errors := [];

  (* Split records and functions *)
  let recs, funs =
    List.fold_right
      (fun frag (rs, fs) ->
         match frag with
         | Ast.RecDecl r -> (r :: rs, fs)
         | Ast.FunDecl f -> (rs, f :: fs))
      prg ([], [])
  in

  (* Two-pass record checking *)
  let record_env_entries, typed_recs = typecheck_records recs in
  let rec_env = Env.make_env ~record_types:record_env_entries [] in

  (* Pass 1 for functions: validate signatures once and check duplicates *)
  let funs_with_types = List.map (fun f -> (f, funtype_of rec_env f)) funs in
  let seen = Hashtbl.create 32 in
  List.iter (fun (name, _) -> Hashtbl.add seen name ()) initial_environment;
  let fun_sign = ref [] in
  List.iter
    (fun (f, ft) ->
       let name = name_of f.Ast.funname in
       if Hashtbl.mem seen name then
         report (Errors.DuplicateFunction { name; loc = loc_of_ident f.Ast.funname })
       else begin
         Hashtbl.add seen name ();
         fun_sign := (name, ft) :: !fun_sign
       end)
    funs_with_types;

  let env =
    Env.make_env ~record_types:record_env_entries (initial_environment @ List.rev !fun_sign)
  in

  (* Verify main entry point *)
  let main_is_found = ref false in
  List.iter
    (fun (f, ft) ->
       if name_of f.Ast.funname = "main" then begin
         main_is_found := true;
         match ft with
         | TAst.FunTyp { ret = TAst.RetTyp TAst.Int; params = [] } -> ()
         | _ ->
           report (Errors.MainBadSignatatur { loc = loc_of_ident f.Ast.funname })
       end)
    funs_with_types;

  if not !main_is_found then begin
    if prg = [] then report (Errors.MissingReturn { loc = None })
    else report Errors.MainMissing
  end;

  (* Pass 2 for functions: typecheck function bodies *)
  let typed_funs = List.map (fun (f, ft) -> typecheck_fundecl env f ft) funs_with_types in

  match !errors with
  | [] -> List.map (fun r -> TAst.RecDecl r) typed_recs @ typed_funs
  | errs -> raise (TypeError (List.rev_map Errors.error_to_string errs))
