(* Semantic analysis for Dolphin, phase 3.

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

let typecheck_typ = function
  | Ast.Int _  -> TAst.Int
  | Ast.Bool _ -> TAst.Bool

let name_of (Ast.Ident {name; _}) = name
let tident (Ast.Ident {name; _}) = TAst.Ident {sym = Symbol.symbol name}
let loc_of_ident (Ast.Ident {loc; _}) = loc

let loc_of_expr = function
  | Ast.Integer {loc; _} | Ast.Boolean {loc; _} | Ast.BinOp {loc; _}
  | Ast.UnOp {loc; _} | Ast.Assignment {loc; _} | Ast.Call {loc; _} -> loc
  | Ast.Lval (Ast.Var id) -> loc_of_ident id

let loc_of_statement = function
  | Ast.VarDeclStm (Ast.DeclBlock {loc; _})
  | Ast.ExprStm {loc; _} | Ast.IfThenElseStm {loc; _} | Ast.WhileStm {loc; _}
  | Ast.ForStm {loc; _} | Ast.BreakStm {loc} | Ast.ContinueStm {loc}
  | Ast.CompoundStm {loc; _} | Ast.ReturnStm {loc; _} -> loc

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

(* ------------------------------------------------------------------------
   Expressions
   ------------------------------------------------------------------------ *)

(* Returns the typed expression and its type. Never raises: errors are
   reported and the expression gets type [ErrorType]. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int; _} -> (TAst.Integer {int}, TAst.Int)
  | Ast.Boolean {bool; _} -> (TAst.Boolean {bool}, TAst.Bool)
  | Ast.BinOp {left; op; right; loc} ->
    let tleft, lt = infertype_expr env left in
    let tright, rt = infertype_expr env right in
    (* If an operand already has an error, don't report a follow-up error. *)
    let check ok =
      if not (is_error lt || is_error rt) && not ok then
        report (Errors.BinOpTypeError {op = binop_to_string op; left = lt; right = rt; loc})
    in
    (* The result type is determined by the operator alone, so an error in an
       operand does not spread to the enclosing expression. *)
    let tp =
      match op with
      | Ast.Plus _ | Ast.Minus _ | Ast.Mul _ | Ast.Div _ | Ast.Rem _ ->
        check (lt = TAst.Int && rt = TAst.Int); TAst.Int
      | Ast.Lt _ | Ast.Le _ | Ast.Gt _ | Ast.Ge _ ->
        check (lt = TAst.Int && rt = TAst.Int); TAst.Bool
      | Ast.Eq _ | Ast.NEq _ ->
        check (lt = rt); TAst.Bool
      | Ast.Land _ | Ast.Lor _ ->
        check (lt = TAst.Bool && rt = TAst.Bool); TAst.Bool
    in
    (TAst.BinOp {left = tleft; op = typecheck_binop op; right = tright; tp}, tp)
  | Ast.UnOp {op; operand; loc} ->
    let toperand, ot = infertype_expr env operand in
    let expected, top =
      match op with
      | Ast.Neg _  -> (TAst.Int, TAst.Neg)
      | Ast.Lnot _ -> (TAst.Bool, TAst.Lnot)
    in
    if not (is_error ot) && ot <> expected then
      report (Errors.UnOpTypeError {op = unop_to_string op; operand = ot; loc});
    (TAst.UnOp {op = top; operand = toperand; tp = expected}, expected)
  | Ast.Lval lval ->
    let tlval, tp = infertype_lval env lval in
    (TAst.Lval tlval, tp)
  | Ast.Assignment {lvl; rhs; loc} ->
    let tlvl, lt = infertype_lval env lvl in
    let trhs, rt = infertype_expr env rhs in
    if not (is_error lt || is_error rt) && lt <> rt then
      report (Errors.TypeMismatch {context = "assignment"; expected = lt; actual = rt; loc});
    (TAst.Assignment {lvl = tlvl; rhs = trhs; tp = lt}, lt)
  | Ast.Call {fname; args; loc} ->
    let tcall, ret = typecheck_call env fname args loc in
    (match ret with
     | TAst.RetTyp tp -> (tcall, tp)
     | TAst.Void ->
       report (Errors.VoidAsValue {name = name_of fname; loc});
       (tcall, TAst.ErrorType))

and infertype_lval env (Ast.Var id) =
  let tp =
    match lookup env id with
    | Some (Env.Var tp) -> tp
    | Some (Env.Fun _) ->
      report (Errors.FunctionAsValue {name = name_of id; loc = loc_of_ident id});
      TAst.ErrorType
    | None ->
      report (Errors.UnboundIdentifier {name = name_of id; loc = loc_of_ident id});
      TAst.ErrorType
  in
  (TAst.Var {ident = tident id; tp}, tp)

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
    (TAst.Call {fname = tident fname; args = typed_args; tp}, tp)
  in
  match lookup env fname with
  | None ->
    report (Errors.UnboundIdentifier {name; loc = loc_of_ident fname}); failed ()
  | Some (Env.Var _) ->
    report (Errors.NotAFunction {name; loc = loc_of_ident fname}); failed ()
  | Some (Env.Fun (TAst.FunTyp {ret; params})) ->
    let param_tps = List.map (fun (TAst.Param {typ; _}) -> typ) params in
    if List.length param_tps <> List.length arg_tps then
      report (Errors.ArgumentCountMismatch
                {name; expected = List.length param_tps; actual = List.length arg_tps; loc})
    else
      List.iteri
        (fun i ((expected, actual), arg) ->
           if not (is_error actual) && actual <> expected then
             report (Errors.ArgumentTypeMismatch
                       {name; index = i + 1; expected; actual; loc = loc_of_expr arg}))
        (List.combine (List.combine param_tps arg_tps) args);
    (TAst.Call {fname = tident fname; args = typed_args; tp = ret}, ret)

(* Checks that [expr] has type [expected]; [context] is used in the message. *)
and typecheck_expr env expr expected context =
  let texpr, actual = infertype_expr env expr in
  if not (is_error actual) && actual <> expected then
    report (Errors.TypeMismatch {context; expected; actual; loc = loc_of_expr expr});
  texpr

(* An expression whose value is discarded: void calls are allowed here. *)
and typecheck_discarded_expr env expr =
  match expr with
  | Ast.Call {fname; args; loc} -> fst (typecheck_call env fname args loc)
  | _ -> fst (infertype_expr env expr)

(* ------------------------------------------------------------------------
   Declarations
   ------------------------------------------------------------------------ *)

(* The initializer is checked before the variable is added to the
   environment. A variable whose type could not be determined gets type
   [ErrorType], so later uses of it do not produce follow-up errors. *)
let typecheck_single_declaration env (Ast.Declaration {name; tp; body; _}) =
  let tbody, body_tp = infertype_expr env body in
  let result_tp =
    match tp with
    | Some t ->
      let expected = typecheck_typ t in
      if not (is_error body_tp) && body_tp <> expected then
        report (Errors.TypeMismatch
                  {context = "declaration of '" ^ name_of name ^ "'";
                   expected; actual = body_tp; loc = loc_of_expr body});
      expected
    | None -> body_tp
  in
  let new_env = Env.insert_local_decl env name result_tp in
  (TAst.Declaration {name = tident name; tp = result_tp; body = tbody}, new_env)

(* Left to right: each variable is visible in the following initializers. *)
let typecheck_declaration_block env (Ast.DeclBlock {declarations; _}) =
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
let rec typecheck_statement env stm =
  match stm with
  | Ast.VarDeclStm decl_block ->
    let typed_block, new_env = typecheck_declaration_block env decl_block in
    (TAst.VarDeclStm typed_block, new_env)

  | Ast.ExprStm {expr; _} ->
    let texpr =
      match expr with
      | None -> None
      | Some (Ast.Call _ as e) -> Some (typecheck_discarded_expr env e)
      | Some (Ast.Assignment _ as e) -> Some (fst (infertype_expr env e))
      | Some e ->
        report (Errors.InvalidExpressionStatement {loc = loc_of_expr e});
        Some (fst (infertype_expr env e))      (* still check it for other errors *)
    in
    (TAst.ExprStm {expr = texpr}, env)

  | Ast.IfThenElseStm {cond; thbr; elbro; _} ->
    let tcond = typecheck_expr env cond TAst.Bool "if condition" in
    let tthbr, _ = typecheck_statement env thbr in
    let telbro = Option.map (fun s -> fst (typecheck_statement env s)) elbro in
    (TAst.IfThenElseStm {cond = tcond; thbr = tthbr; elbro = telbro}, env)

  | Ast.CompoundStm {stms; _} ->
    let tstms, _ = typecheck_statement_seq env stms in
    (TAst.CompoundStm {stms = tstms}, env)

  | Ast.ReturnStm {ret; _} ->
    (TAst.ReturnStm {ret = typecheck_expr env ret TAst.Int "return value"}, env)

  | Ast.WhileStm {cond; body; _} ->
    let tcond = typecheck_expr env cond TAst.Bool "while condition" in
    let tbody, _ = typecheck_statement (Env.enter_loop env) body in
    (TAst.WhileStm {cond = tcond; body = tbody}, env)

  | Ast.ForStm {init; cond; update; body; _} ->
    (* Declarations in init are visible in cond, update and body only. *)
    let typed_init, for_env =
      match init with
      | None -> (None, env)
      | Some fi -> let tfi, e = typecheck_for_init env fi in (Some tfi, e)
    in
    let typed_cond =
      Option.map (fun c -> typecheck_expr for_env c TAst.Bool "for condition") cond in
    let typed_update = Option.map (typecheck_discarded_expr for_env) update in
    let typed_body, _ = typecheck_statement (Env.enter_loop for_env) body in
    (TAst.ForStm {init = typed_init; cond = typed_cond;
                  update = typed_update; body = typed_body}, env)

  | Ast.BreakStm {loc} ->
    if not (Env.is_inside_loop env) then report (Errors.BreakOutsideLoop {loc});
    (TAst.BreakStm, env)

  | Ast.ContinueStm {loc} ->
    if not (Env.is_inside_loop env) then report (Errors.ContinueOutsideLoop {loc});
    (TAst.ContinueStm, env)

and typecheck_statement_seq env stms =
  let result_env, tstms =
    List.fold_left_map
      (fun env s -> let ts, env' = typecheck_statement env s in (env', ts))
      env stms
  in
  (tstms, result_env)

(* ------------------------------------------------------------------------
   Programs
   ------------------------------------------------------------------------ *)

let initial_environment = Env.make_env [
  ("print_integer",
    TAst.FunTyp {ret = TAst.Void;
                 params = [TAst.Param {paramname = TAst.Ident {sym = Symbol.symbol "value"};
                                       typ = TAst.Int}]});
  ("read_integer",
    TAst.FunTyp {ret = TAst.RetTyp TAst.Int; params = []});
]

(* Checks the whole program and the requirement that it ends in a return.
   Raises [TypeError] with all error messages, in source order, if any
   error was found. *)
let typecheck_prog prg =
  errors := [];
  let tstms, _ = typecheck_statement_seq initial_environment prg in
  (match List.rev prg with
   | Ast.ReturnStm _ :: _ -> ()
   | [] -> report (Errors.MissingReturn {loc = None})
   | last :: _ -> report (Errors.MissingReturn {loc = Some (loc_of_statement last)}));
  match !errors with
  | [] -> tstms
  | errs -> raise (TypeError (List.rev_map Errors.error_to_string errs))