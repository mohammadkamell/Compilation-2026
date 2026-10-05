exception TypeError of string list

module TAst = TypedAst

let typecheck_typ = function
  | Ast.Int  -> TAst.Int
  | Ast.Bool -> TAst.Bool

let tident (Ast.Ident {name}) = TAst.Ident {sym = Symbol.symbol name}

let typecheck_binop = function
  | Ast.Plus  -> TAst.Plus  | Ast.Minus -> TAst.Minus | Ast.Mul -> TAst.Mul
  | Ast.Div   -> TAst.Div   | Ast.Rem   -> TAst.Rem   | Ast.Lt  -> TAst.Lt
  | Ast.Le    -> TAst.Le    | Ast.Gt    -> TAst.Gt    | Ast.Ge  -> TAst.Ge
  | Ast.Lor   -> TAst.Lor   | Ast.Land  -> TAst.Land  | Ast.Eq  -> TAst.Eq
  | Ast.NEq   -> TAst.NEq

let typecheck_unop = function
  | Ast.Neg  -> TAst.Neg
  | Ast.Lnot -> TAst.Lnot

(* should return a pair of a typed expression and its inferred type. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int} -> (TAst.Integer {int}, TAst.Int)
  | Ast.Boolean {bool} -> (TAst.Boolean {bool}, TAst.Bool)
  | Ast.BinOp {left; op; right} ->
    let tleft,  tleft_tp  = infertype_expr env left  in
    let tright, tright_tp = infertype_expr env right in
    let top = typecheck_binop op in
    (match op, tleft_tp, tright_tp with
     | (Ast.Plus | Ast.Minus | Ast.Mul | Ast.Div | Ast.Rem), TAst.Int, TAst.Int ->
       (TAst.BinOp {left = tleft; op = top; right = tright; tp = TAst.Int}, TAst.Int)
     | (Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge), TAst.Int, TAst.Int ->
       (TAst.BinOp {left = tleft; op = top; right = tright; tp = TAst.Bool}, TAst.Bool)
     | (Ast.Eq | Ast.NEq), t1, t2 when t1 = t2 ->
       (TAst.BinOp {left = tleft; op = top; right = tright; tp = TAst.Bool}, TAst.Bool)
     | (Ast.Land | Ast.Lor), TAst.Bool, TAst.Bool ->
       (TAst.BinOp {left = tleft; op = top; right = tright; tp = TAst.Bool}, TAst.Bool)
     | _ -> failwith "type error in Binop")
  | Ast.UnOp {op; operand} ->
    let toperand, exp_t = infertype_expr env operand in
    let top = typecheck_unop op in
    (match op, exp_t with
     | Ast.Neg,  TAst.Int  -> (TAst.UnOp {op = top; operand = toperand; tp = TAst.Int},  TAst.Int)
     | Ast.Lnot, TAst.Bool -> (TAst.UnOp {op = top; operand = toperand; tp = TAst.Bool}, TAst.Bool)
     | _ -> failwith "type error in UnOp")
  | Ast.Lval lval ->
    let tlval, tp = infertype_lval env lval in
    (TAst.Lval tlval, tp)
  | Ast.Assignment {lvl; rhs} ->
    let tlvl, lvl_tp = infertype_lval env lvl in
    let trhs, rhs_tp = infertype_expr env rhs in
    if lvl_tp = rhs_tp then
      (TAst.Assignment {lvl = tlvl; rhs = trhs; tp = rhs_tp}, lvl_tp)
    else
      failwith "type mismatch in assignment"
  | Ast.Call {fname; args} ->
    (match Env.lookup_var_fun env fname with
     | Env.Fun (TAst.FunTyp {ret = (TAst.RetTyp t as ret); params}) ->
       let t_args   = List.map (infertype_expr env) args in
       let t_params = List.map (fun (TAst.Param {typ; _}) -> typ) params in
       if List.map snd t_args <> t_params then
         failwith "error in Call"
       else
         (TAst.Call {fname = tident fname; args = List.map fst t_args; tp = ret}, t)
     | Env.Fun _ -> failwith "void call cannot be used as a value"
     | Env.Var _ -> failwith "only functions can be called")

and infertype_lval env lvl =
  match lvl with
  | Ast.Var id ->
    (match Env.lookup_var_fun env id with
     | Env.Var t -> (TAst.Var {ident = tident id; tp = t}, t)
     | Env.Fun _ -> failwith "A function cannot be used as a value")

(* checks that an expression has the required type tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp then failwith "type mismatch";
  texpr

(* --------------------------------------------------------------------------
   Declaration helpers
   -------------------------------------------------------------------------- *)

(* Typecheck a single declaration.  The initializer is evaluated against
   [env] (before the variable is inserted), then the variable is added. *)
and typecheck_single_declaration env (Ast.Declaration {name; tp; body}) =
  let tbody, tbody_tp = infertype_expr env body in
  let result_tp =
    match tp with
    | Some t ->
      let expected_tp = typecheck_typ t in
      if tbody_tp <> expected_tp then
        failwith "Variable has the wrong type"
      else
        expected_tp
    | None ->
      (* inferred — reject Void/ErrorType (can't happen here, but guard anyway) *)
      tbody_tp
  in
  let new_env = Env.insert_local_decl env name result_tp in
  (TAst.Declaration {name = tident name; tp = result_tp; body = tbody}, new_env)

(* Process a list of declarations left-to-right; each declaration's
   variable becomes visible to subsequent initializers. *)
and typecheck_declaration_block env (Ast.DeclBlock declarations) =
  let typed_declarations, final_env =
    List.fold_left
      (fun (typed_acc, current_env) declaration ->
        let typed_decl, next_env = typecheck_single_declaration current_env declaration in
        (typed_decl :: typed_acc, next_env))
      ([], env)
      declarations
  in
  (TAst.DeclBlock (List.rev typed_declarations), final_env)

(* --------------------------------------------------------------------------
   For-init helper
   -------------------------------------------------------------------------- *)

and typecheck_for_init env = function
  | Ast.FIExpr expr ->
    (* expression just needs to be well-typed; any type is OK *)
    let typed_expr, _ = infertype_expr env expr in
    (TAst.FIExpr typed_expr, env)
  | Ast.FIDecl decl_block ->
    let typed_block, new_env = typecheck_declaration_block env decl_block in
    (TAst.FIDecl typed_block, new_env)

(* --------------------------------------------------------------------------
   Statement checking
   -------------------------------------------------------------------------- *)

(* should check the validity of a statement and produce the corresponding
   typed statement. Returns (typed_statement, updated_env). *)
and typecheck_statement env stm =
  match stm with

  (* --- variable declarations --- *)
  | Ast.VarDeclStm decl_block ->
    let typed_block, new_env = typecheck_declaration_block env decl_block in
    (TAst.VarDeclStm typed_block, new_env)

  (* --- expression statements (assignments / calls only) --- *)
  | Ast.ExprStm {expr} ->
    let tex =
      match expr with
      | Some (Ast.Call {fname; args}) ->
        (match Env.lookup_var_fun env fname with
         | Env.Fun (TAst.FunTyp {ret; params}) ->
           let t_args   = List.map (infertype_expr env) args in
           let t_params = List.map (fun (TAst.Param {typ; _}) -> typ) params in
           if List.map snd t_args <> t_params then
             failwith "error: arguments do not match function parameters"
           else
             Some (TAst.Call {fname = tident fname; args = List.map fst t_args; tp = ret})
         | Env.Var _ -> failwith "only functions can be called")
      | Some (Ast.Assignment _ as e) -> Some (fst (infertype_expr env e))
      | Some _ -> failwith "only assign and calls are valid statements"
      | None -> None
    in
    (TAst.ExprStm {expr = tex}, env)

  (* --- if/then/else --- *)
  | Ast.IfThenElseStm {cond; thbr; elbro} ->
    let tcond = typecheck_expr env cond TAst.Bool in
    let tthbr, _ = typecheck_statement env thbr in
    let tebro =
      match elbro with
      | Some stm ->
        let tstm, _ = typecheck_statement env stm in
        Some tstm
      | None -> None
    in
    (TAst.IfThenElseStm {cond = tcond; thbr = tthbr; elbro = tebro}, env)

  (* --- compound block --- *)
  | Ast.CompoundStm {stms} ->
    let tstms, _ = typecheck_statement_seq env stms in
    (TAst.CompoundStm {stms = tstms}, env)

  (* --- return --- *)
  | Ast.ReturnStm {ret} ->
    (TAst.ReturnStm {ret = typecheck_expr env ret TAst.Int}, env)

  (* --- while --- *)
  | Ast.WhileStm {cond; body} ->
    let tcond = typecheck_expr env cond TAst.Bool in
    let loop_env  = Env.enter_loop env in
    let tbody, _  = typecheck_statement loop_env body in
    (* loop_env is discarded; outer env is unchanged *)
    (TAst.WhileStm {cond = tcond; body = tbody}, env)

  (* --- for --- *)
  | Ast.ForStm {init; cond; update; body} ->
    (* Step 1: process initializer, obtaining for_env *)
    let typed_init, for_env =
      match init with
      | None ->
        (None, env)
      | Some fi ->
        let tfi, e = typecheck_for_init env fi in
        (Some tfi, e)
    in
    (* Step 2: optional condition must be bool *)
    let typed_cond =
      match cond with
      | None      -> None
      | Some expr -> Some (typecheck_expr for_env expr TAst.Bool)
    in
    (* Step 3: optional update – any valid type, no restriction *)
    let typed_update =
      match update with
      | None      -> None
      | Some expr ->
        let texpr, _ = infertype_expr for_env expr in
        Some texpr
    in
    (* Step 4: body checked in loop context built from for_env *)
    let body_env  = Env.enter_loop for_env in
    let typed_body, _ = typecheck_statement body_env body in
    (* for_env (and its declarations) are discarded; return outer env *)
    (TAst.ForStm {init = typed_init; cond = typed_cond;
                  update = typed_update; body = typed_body}, env)

  (* --- break --- *)
  | Ast.BreakStm ->
    if Env.is_inside_loop env then
      (TAst.BreakStm, env)
    else
      failwith "break statement outside of a loop"

  (* --- continue --- *)
  | Ast.ContinueStm ->
    if Env.is_inside_loop env then
      (TAst.ContinueStm, env)
    else
      failwith "continue statement outside of a loop"

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  let result_env, tstms =
    List.fold_left_map
      (fun env s ->
        let ts, env' = typecheck_statement env s in
        (env', ts))
      env stms
  in
  (tstms, result_env)

(* the initial environment includes all library functions, no local variables. *)
let initial_environment = Env.make_env [
  ("print_integer",
    TAst.FunTyp {ret = TAst.Void;
                 params = [TAst.Param {paramname = TAst.Ident {sym = Symbol.symbol "n"}; typ = TAst.Int}]});
  ("read_integer",
    TAst.FunTyp {ret = TAst.RetTyp TAst.Int; params = []});
]

(* should check that the program ends in a return statement and all
   statements are valid. *)
let typecheck_prog prg =
  let tstms, _ = typecheck_statement_seq initial_environment prg in
  match List.rev tstms with
  | TAst.ReturnStm _ :: _ -> tstms
  | [] -> failwith "program is empty"
  | _  -> failwith "program must have a return statement"
