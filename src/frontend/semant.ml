exception TypeError of string list

module TAst = TypedAst

let typecheck_typ = function
| Ast.Int -> TAst.Int
| Ast.Bool -> TAst.Bool

let tident (Ast.Ident {name}) = TAst.Ident {sym = Symbol.symbol name}

let typecheck_binop = function
  | Ast.Plus -> TAst.Plus | Ast.Minus -> TAst.Minus | Ast.Mul -> TAst.Mul
  | Ast.Div -> TAst.Div | Ast.Rem -> TAst.Rem | Ast.Lt -> TAst.Lt
  | Ast.Le -> TAst.Le | Ast.Gt -> TAst.Gt | Ast.Ge -> TAst.Ge
  | Ast.Lor -> TAst.Lor | Ast.Land -> TAst.Land | Ast.Eq -> TAst.Eq
  | Ast.NEq -> TAst.NEq

let typecheck_unop = function
  | Ast.Neg -> TAst.Neg
  | Ast.Lnot -> TAst.Lnot

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =   
  match expr with   
  | Ast.Integer {int} -> (TAst.Integer {int}, TAst.Int)   
  | Ast.Boolean {bool} -> (TAst.Boolean {bool}, TAst.Bool)   
  | Ast.BinOp {left; op; right} -> (
    let tleft, tleft_tp = infertype_expr env left in
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
    | _ -> failwith "type error in Binop"))
 | Ast.UnOp {op; operand} -> (
    let toperand, exp_t = infertype_expr env operand in
    let top = typecheck_unop op in
    (match op, exp_t with
    | Ast.Neg, TAst.Int ->
      (TAst.UnOp {op = top; operand = toperand; tp = TAst.Int}, TAst.Int)
    | Ast.Lnot, TAst.Bool ->
      (TAst.UnOp {op = top; operand = toperand; tp = TAst.Bool}, TAst.Bool)
    | _ -> failwith "type error in UnOp"))  
  | Ast.Lval lval ->        
      let tlval, tp = infertype_lval env lval in       
      (TAst.Lval tlval, tp)
  | Ast.Assignment {lvl; rhs} -> 
      let tlvl, lvl_tp = infertype_lval env lvl in
      let trhs, rhs_tp = infertype_expr env rhs in
      if lvl_tp = rhs_tp then
        (TAst.Assignment {lvl = tlvl; rhs = trhs; tp = rhs_tp}, lvl_tp)
      else
        (failwith "error")
| Ast.Call {fname; args} ->
  (match Env.lookup_var_fun env fname with
   | Env.Fun (TAst.FunTyp {ret = (TAst.RetTyp t as ret); params}) ->
     let t_args = List.map (infertype_expr env) args in
     let t_param = List.map (fun (TAst.Param {typ; _}) -> typ) params in
     if List.map snd t_args <> t_param then
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
(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
if texprtp <> tp then failwith "type mismatch";
  texpr

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
| Ast.VarDeclStm {name; tp; body} -> (
  let tbody, tfbody = infertype_expr env body in
  let result_tp = match tp with
    | Some t ->
      let expected_tp = typecheck_typ t in
      if tfbody <> expected_tp then
        failwith "Variable has the wrong type"
      else
        expected_tp
    | None -> tfbody
  in
  let new_env = Env.insert_local_decl env name result_tp in
  (TAst.VarDeclStm {name = tident name; tp = result_tp; body = tbody}, new_env)
)
| Ast.ExprStm {expr} -> (
  let tex = match expr with
    | Some (Ast.Call {fname; args}) -> (
      match Env.lookup_var_fun env fname with
      | Env.Fun (TAst.FunTyp {ret; params}) ->
        let t_args = List.map (infertype_expr env) args in
        let t_param = List.map (fun (TAst.Param {typ; _}) -> typ) params in
        if List.map snd t_args <> t_param then
          failwith "error: arguments do not match function parameters"
        else
          Some (TAst.Call {fname = tident fname; args = List.map fst t_args; tp = ret})
      | Env.Var _ -> failwith "only functions can be called")
    | Some (Ast.Assignment _ as e) -> Some (fst (infertype_expr env e))
    | Some _ -> failwith "only assign and calls are valid statements"
    | None -> None
  in
  (TAst.ExprStm {expr = tex}, env)
)
| Ast.IfThenElseStm {cond; thbr; elbro} -> (
  let tcond = typecheck_expr env cond TAst.Bool in
  let tthbr, _ = typecheck_statement env thbr in
  let tebro = match elbro with
    | Some stm ->
      let tstm, _ = typecheck_statement env stm in
      Some tstm
    | None -> None
  in
  (TAst.IfThenElseStm {cond = tcond; thbr = tthbr; elbro = tebro}, env)
)
  | Ast.CompoundStm {stms} -> (
    let tstms, _ = typecheck_statement_seq env stms in
    (TAst.CompoundStm {stms = tstms}, env)
  )
| Ast.ReturnStm {ret} ->
  (TAst.ReturnStm {ret = typecheck_expr env ret TAst.Int}, env)

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =  
  let result_env, tstms = 
  List.fold_left_map 
  (fun env s -> let ts, env' = typecheck_statement env s in (env', ts)) env stms in
  (tstms, result_env)

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = Env.make_env [
  ("print_integer",
    TAst.FunTyp {ret = TAst.Void;
                 params = [TAst.Param {paramname = TAst.Ident {sym = Symbol.symbol "n"}; typ = TAst.Int}]});
  ("read_integer",
    TAst.FunTyp {ret = TAst.RetTyp TAst.Int; params = []});
]

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg =
  let tstms, _ = typecheck_statement_seq initial_environment prg in
  match List.rev tstms with
  | TAst.ReturnStm _ :: _ -> tstms
  | [] -> failwith "program is empty"
  | _ -> failwith "program must have a return statement"
