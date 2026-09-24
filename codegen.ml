module L = Ll
module T = TypedAst
module B = CfgBuilder
module Sym = Symbol


(* Environment : Maps a source variavble to register + llvm type*)
type env = (L.uid * L.ty) Sym.Table.t
let empty_env : env = Sym.Table.empty

(* Fresh ensures fresh names for variables and temps. *)
let counter = ref 0
let fresh (prefix : string ) : Sym.symbol = 
  incr counter;
  Sym.symbol (prefix ^"_"^ string_of_int !counter)


(* Type mapping (bottom up approach)*)
let ll_type_of_typ (tp: T.typ) : L.ty = 
  match tp with 
  | T.Int -> L.I64
  | T.Bool -> L.I1
  | T.ErrorType -> failwith "Codegen: ErrorType reached codegen (bug in semant)"

let ll_type_of_rettyp (rt : T.rettyp) : L.ty = 
  match rt with
  | T.Void -> L.Void
  | T.RetTyp tp -> ll_type_of_typ tp


let ll_bop_of_binop (op : T.binop) : L.bop =
  match op with
  | T.Plus  -> L.Add
  | T.Minus -> L.Sub
  | T.Mul   -> L.Mul
  | T.Div   -> L.SDiv
  | T.Rem   -> L.SRem
  | _ -> failwith "ll_bop_of_binop: not an arithmetic operator"

let ll_cnd_of_binop (op : T.binop) : L.cnd =
  match op with
  | T.Eq  -> L.Eq
  | T.NEq -> L.Ne
  | T.Lt  -> L.Slt
  | T.Le  -> L.Sle
  | T.Gt  -> L.Sgt
  | T.Ge  -> L.Sge
  | _ -> failwith "ll_cnd_of_binop: not a comparison"


let rec type_of_expr (e : T.expr) : T.rettyp =
  match e with
  | T.Integer _ -> T.RetTyp T.Int
  | T.Boolean _ -> T.RetTyp T.Bool
  | T.BinOp {tp; _} -> T.RetTyp tp
  | T.UnOp {tp; _} -> T.RetTyp tp
  | T.Lval lvl -> type_of_lval lvl
  | T.Assignment {tp; _} -> T.RetTyp tp
  | T.Call {tp; _} -> tp
and type_of_lval (l : T.lval) : T.rettyp =
  match l with
  | T.Var {tp; _} -> T.RetTyp tp

let rec trans_expr (env: env) (e: T.expr) (b: B.cfg_builder) : B.cfg_builder * L.operand = 
  match e with 
  | T.Integer {int} -> (b, L.IConst64 int)
  | T.Boolean {bool} -> (b, L.BConst bool)
  | T.BinOp {left; op; right; _} ->
    (match op with
     | T.Plus | T.Minus | T.Mul ->
         let (b1, o1) = trans_expr env left b in
         let (b2, o2) = trans_expr env right b1 in
         let temp = fresh "temp" in
         let b3 = B.add_insn (Some temp, L.Binop (ll_bop_of_binop op, L.I64, o1, o2)) b2 in
         (b3, L.Id temp)
     | T.Lt | T.Le | T.Gt | T.Ge | T.Eq | T.NEq ->
         let (b1, o1) = trans_expr env left b in
         let (b2, o2) = trans_expr env right b1 in
         let operand_ty = ll_type_of_rettyp (type_of_expr left) in
         let temp = fresh "temp" in
         let b3 = B.add_insn (Some temp, L.Icmp (ll_cnd_of_binop op, operand_ty, o1, o2)) b2 in
         (b3, L.Id temp)
     | T.Div | T.Rem -> (* We evaluate lhs then rhs. Afterwards we check if o2 is 0, fail by calling C function*)
         let (b1, o1) = trans_expr env left b in 
         let (b2, o2) = trans_expr env right b1 in 
         let is_zero = fresh "is_zero" in
         let b3 = B.add_insn (Some is_zero, L.Icmp (L.Eq, L.I64, o2, L.IConst64 0L)) b2 in
         let err_lbl = fresh "div_error" in 
         let ok_lbl = fresh "div_ok" in
         let b4 = B.term_block (L.Cbr (L.Id is_zero, err_lbl, ok_lbl)) b3 in
         (* Error block *)
         let b5 = B.start_block err_lbl b4 in
         let b6 = B.add_insn (None, L.Call (L.Void, L.Gid (Sym.symbol "dolphin_div_by_zero"), [])) b5 in
         let b7 = B.term_block L.Unreachable b6 in
         (* Ok block *)
         let b8 = B.start_block ok_lbl b7 in
         let temp = fresh "temp" in 
         let b9 = B.add_insn (Some temp, L.Binop (ll_bop_of_binop op, L.I64, o1, o2)) b8 in 
         (b9, L.Id temp)
     | T.Lor | T.Land ->
         let res = fresh "sc_res" in
         let b1 = B.add_alloca (res, L.I1) b in
         let (b2, o1) = trans_expr env left b1 in
         let b3 = B.add_insn (None, L.Store (L.I1, o1, L.Id res)) b2 in
         let rhs_lbl = fresh "sc_rhs" in
         let done_lbl = fresh "sc_done" in 
         let cbr = 
            if op = T.Land then L.Cbr (o1, rhs_lbl, done_lbl) (* if && and A true   -> then evaluate B*)
            else                L.Cbr (o1, done_lbl, rhs_lbl) (* if || and A false  -> then evaluate B*)
          in
          let b4 = B.term_block cbr b3 in
          (* RHS *)
          let b5 = B.start_block rhs_lbl b4 in
          let (b6, o2) = trans_expr env right b5 in
          let b7 = B.add_insn (None, L.Store (L.I1, o2, L.Id res)) b6 in
          let b8 = B.term_block (L.Br done_lbl) b7 in
          (* Merge *)
          let b9 = B.start_block done_lbl b8 in
          let temp = fresh "temp" in
          let b10 = B.add_insn (Some temp, L.Load (L.I1, L.Id res)) b9 in   
         (b10, L.Id temp))
  | T.UnOp {op; operand; _} -> 
      let b1, o = trans_expr env operand b in 
      let temp = fresh "temp" in 
      let b2  = 
        (match op with 
      | T.Neg -> B.add_insn(Some temp, L.Binop(L.Sub, L.I64, L.IConst64 0L, o)) b1
      | T.Lnot -> B.add_insn(Some temp, L.Binop(L.Xor, L.I1, o, L.BConst true)) b1
      )
      in 
      (b2, L.Id temp)
  | T.Call {fname = T.Ident {sym}; args; tp} -> 
      let (b1, ops) = trans_args env args b in 
      let instruction = L.Call( ll_type_of_rettyp tp, L.Gid sym, ops) in 
      (match tp with
      | T.Void -> 
          let b2 = B.add_insn (None, instruction) b1 in 
          (b2, L.Null)
      | T.RetTyp _ -> 
          let temp = fresh "temp" in 
          let b2 = B.add_insn (Some temp, instruction) b1 in 
          (b2, L.Id temp)
        )
  |T.Lval (T.Var {ident = T.Ident {sym}; _}) -> 
      let (slot, llty) = Sym.Table.find sym env in 
      let temp = fresh "temp" in 
      let b1 = B.add_insn (Some temp, L.Load (llty, L.Id slot)) b in 
      (b1, L.Id temp)
  | T.Assignment {lvl = T.Var {ident = T.Ident {sym}; _}; rhs; _} ->
      let (b1, o) = trans_expr env rhs b in 
      let (slot, llty) = Sym.Table.find sym env in
      let b2 = B.add_insn (None, L.Store (llty, o, L.Id slot )) b1 in
      (b2, o) 
and trans_args (env: env) (args: T.expr list) (b : B.cfg_builder) : B.cfg_builder * (L.ty * L.operand) list = 
match args with 
| [] -> (b, [])
| arg :: rest -> 
  let (b1, o) = trans_expr env arg b in 
  let ty = ll_type_of_rettyp (type_of_expr arg) in 
  let (b2, rest_ops) = trans_args env rest b1 in 
  (b2, (ty, o) :: rest_ops)



let rec trans_stmt (env: env) (s: T.statement) (b: B.cfg_builder) : env * B.cfg_builder = 
  match s with 
  | T.ReturnStm {ret} ->
    let (b1, op) = trans_expr env ret b in
    let b2 = B.term_block (L.Ret (L.I64, Some op)) b1 in
    let b3 = B.start_block (fresh "after_return") b2 in
    (env, b3)
  | T.ExprStm {expr} -> 
      (match expr with
      | Some e -> 
        let (b1, _) = trans_expr env e b in 
        (env, b1)
      | None ->  (env, b) )
  | T.VarDeclStm {name = T.Ident {sym}; tp; body} -> 
      let (b1, op1) = trans_expr env body b in 
      let ll_typ = ll_type_of_typ tp in 
      let fresh_slot = fresh (Sym.name sym) in 
      let b2 = B.add_alloca (fresh_slot, ll_typ) b1 in
      let b3 = B.add_insn (None, L.Store (ll_typ, op1, L.Id fresh_slot)) b2 in 
      let new_env = Sym.Table.add sym (fresh_slot, ll_typ) env in 
      (new_env, b3)
  | T.CompoundStm {stms} -> 
      let (_, b1) = 
        List.fold_left (fun (env', b') s -> trans_stmt env' s b') (env,b) stms in 
      (env, b1)
  | T.IfThenElseStm {cond; thbr; elbro} -> 
      let (b1, cond_op) = trans_expr env cond b in 
      let then_lbl = fresh "then" in 
      let else_lbl = fresh "else" in 
      let merge_lbl = fresh "merge" in 
      let b2 = B.term_block (L.Cbr (cond_op, then_lbl, else_lbl)) b1 in 
      (* then branch *)
      let b3 = B.start_block then_lbl b2 in 
      let (_, b4) = trans_stmt env thbr b3 in 
      let b5 = B.term_block (L.Br merge_lbl) b4 in 
      (*else branch *)
      let b6 = B.start_block else_lbl b5 in 
      let b7 = 
        (match elbro with 
        | Some elbr -> let (_,b') = trans_stmt env elbr b6 in b'
        | None -> b6) in
      let b8 = B.term_block (L.Br merge_lbl) b7 in 
      let b9 = B.start_block merge_lbl b8 in 
      (env, b9)  




let codegen_prog (prog: T.program) : L.prog = 
  let (_, final_b) = List.fold_left (fun (env, b) s -> trans_stmt env s b)
  (empty_env, B.empty_cfg_builder) prog in 
  let cfg = B.get_cfg final_b in 
  { L.tdecls = [];
    extgdecls = [];
    gdecls = []; 
    extfuns = [ (Sym.symbol "print_integer", ([L.I64], L.Void));
                (Sym.symbol "read_integer", ([], L.I64));
                (Sym.symbol "dolphin_div_by_zero", ([], L.Void)) ];
    fdecls = [ (Sym.symbol "dolphin_main", 
                  {fty = ([], L.I64); param = []; cfg})] 
  }
