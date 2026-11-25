module L = Ll
module T = TypedAst
module B = CfgBuilder
module Sym = Symbol


(* ---------------------------------------------- 
   Runtime and standard library (phase 5)
   ---------------------------------------------- *)
let ptr = L.Ptr (* printed as "ptr": LLVM pointers are untyped *)

(* Signatures of all external functions: *)
let external_functions : (string * L.fty) list = [
  (* runtime *)
  "compare_strings",                        ([ptr; ptr], L.I64);
  "allocate_record",                        ([L.I32], ptr);
  "raw_allocate_on_heap",                   ([L.I32], ptr);
  "allocate_array",                         ([L.I32; L.I64; ptr], ptr);
  "report_error_array_index_out_of_bounds", ([], L.Void);
  "report_error_nil_access",                ([], L.Void);
  "report_error_division_by_zero",          ([], L.Void);
  (* standard library *)
  "bytes_array_to_string", ([ptr], ptr);
  "string_to_bytes_array", ([ptr], ptr);
  "byte_to_int_unsigned",  ([L.I8], L.I64);
  "byte_to_int_signed",    ([L.I8], L.I64);
  "int_to_byte_unsigned",  ([L.I64], L.I8);
  "int_to_byte_signed",    ([L.I64], L.I8);
  "ascii_ord",             ([ptr], L.I64);
  "ascii_chr",             ([L.I64], ptr);
  "string_concat",         ([ptr; ptr], ptr);
  "substring",             ([ptr; L.I64; L.I64], ptr);
  "int_to_string",         ([L.I64], ptr);
  "string_to_int",         ([ptr], L.I64);
  "input_byte",            ([ptr], L.I64);
  "output_byte",           ([L.I8; ptr], L.I1);
  "input_bytes_array",     ([L.I64; ptr], ptr);
  "output_bytes_array",    ([ptr; ptr], L.Void);
  "output_string",         ([ptr; ptr], L.Void);
  "seek_in_file",          ([L.I64; L.I1; ptr], L.I1);
  "pos_in_file",           ([ptr], L.I64);
  "close_file",            ([ptr], L.I1);
  "flush_file",            ([ptr], L.I1);
  "error_in_file",         ([ptr], L.I1);
  "end_of_file",           ([ptr], L.I1);
  "get_eof",               ([], L.I64);
  "open_file",             ([ptr; ptr], ptr);
  "get_stdin",             ([], ptr);
  "get_stderr",            ([], ptr);
  "get_stdout",            ([], ptr);
  "get_cmd_args",          ([], ptr);
  "exit",                  ([L.I64], L.Void);
  (* kept from earlier phases (still in stdlib.c) *)
  "print_integer",         ([L.I64], L.Void);
  "read_integer",          ([], L.I64);
]



(* Environment : Maps a source variavble to register + llvm type*)
type env = (L.uid * L.ty) Sym.Table.t
let empty_env : env = Sym.Table.empty

(* Fresh ensures fresh names for variables and temps. *)
let counter = ref 0
let fresh (prefix : string ) : Sym.symbol = 
  incr counter;
  Sym.symbol (prefix ^"_"^ string_of_int !counter)

(* Global data (string literals) collected during code generation. *)
let globals : (L.gid * L.gdecl) list ref = ref []


(* Records: name -> fields (name, type) in declaration order.
   Filled from the RecDecls at the start of codegen_prog. *)
let records : (string, (string * T.typ) list) Hashtbl.t = Hashtbl.create 16

let record_tid (s : Sym.symbol) : L.tid = Sym.symbol ("dolphin_record_" ^ Sym.name s)

(* Index and type of a field: the index is its position in the declaration. *)
let field_info (T.RecordName {sym = r}) (T.FieldName {sym = f}) : int * T.typ =
  let rec find i = function
    | [] -> failwith ("Codegen: unknown field " ^ Sym.name f)
    | (name, tp) :: rest -> if name = Sym.name f then (i, tp) else find (i + 1) rest
  in
  find 0 (Hashtbl.find records (Sym.name r))

(* External functions keep their C names; all Dolphin functions, including
   main, get the prefix dolphin_fun_ (the runtime calls dolphin_fun_main). *)
let llvm_fun_name (name : string) : string =
  if List.mem_assoc name external_functions then name
  else "dolphin_fun_" ^ name


(* Type mapping (bottom up approach)*)
let ll_type_of_typ (tp: T.typ) : L.ty =
  match tp with
  | T.Int -> L.I64
  | T.Bool -> L.I1
  | T.Byte -> L.I8
  (* strings, arrays and records are all represented by pointers *)
  | T.Str | T.Array _ | T.Record _ | T.NilType -> L.Ptr
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
  | T.CommaExpr {tp; _} -> T.RetTyp tp
  | T.String _ -> T.RetTyp T.Str
  | T.LengthOf _ -> T.RetTyp T.Int
  | T.Rcrd {recname; _} -> T.RetTyp (T.Record recname)
  | T.Arr {elemtp; _} -> T.RetTyp (T.Array elemtp)
  | T.Nil -> T.RetTyp T.NilType
and type_of_lval (l : T.lval) : T.rettyp =
  match l with
  | T.Var {tp; _} -> T.RetTyp tp
  | T.Idx {tp; _} | T.Fld {tp; _} -> T.RetTyp tp


(* Emits a check that pointer p is not nil; if it is, report a runtime error.
   Returns the builder positioned in the block where p is known to be non-nil. *)
let check_not_nil (p : L.operand) (b : B.cfg_builder) : B.cfg_builder =
  let is_nil = fresh "is_nil" in
  let b1 = B.add_insn (Some is_nil, L.Icmp (L.Eq, L.Ptr, p, L.Null)) b in
  let err_lbl = fresh "nil_error" in
  let ok_lbl = fresh "nil_ok" in
  let b2 = B.term_block (L.Cbr (L.Id is_nil, err_lbl, ok_lbl)) b1 in
  let b3 = B.start_block err_lbl b2 in
  let b4 = B.add_insn (None, L.Call (L.Void, L.Gid (Sym.symbol "report_error_nil_access"), [])) b3 in
  let b5 = B.term_block L.Unreachable b4 in
  B.start_block ok_lbl b5

(* Size in bytes of an LLVM type, computed by LLVM itself (GEP size hack). *)
let size_of (ty : L.ty) (b : B.cfg_builder) : B.cfg_builder * L.operand =
  let size_ptr = fresh "size_ptr" in
  let b1 = B.add_insn (Some size_ptr, L.Gep (ty, L.Null, [L.IConst32 1l])) b in
  let size = fresh "size" in
  let b2 = B.add_insn (Some size, L.Ptrtoint (L.Id size_ptr, L.I32)) b1 in
  (b2, L.Id size)


(* The default value of array elements of the given type. *)
let default_value (tp : T.typ) : L.operand =
  match tp with
  | T.Int -> L.IConst64 0L
  | T.Bool -> L.IConst1 false
  | T.Byte -> L.IConst8 '\000'
  | T.Str -> L.Gid (Sym.symbol "dolphin_rc_empty_string")
  | T.Array _ | T.Record _ | T.NilType -> L.Null
  | T.ErrorType -> failwith "Codegen: ErrorType reached codegen (bug in semant)"


(* Emits a check that 0 <= i < length of array p; otherwise reports a runtime error. *)
let check_bounds (p : L.operand) (i : L.operand) (b : B.cfg_builder) : B.cfg_builder =
  let len_ptr = fresh "len_ptr" in
  let b1 = B.add_insn (Some len_ptr,
             L.Gep (L.Namedt (Sym.symbol "array_type"), p, [L.IConst32 0l; L.IConst32 0l])) b in
  let len = fresh "len" in
  let b2 = B.add_insn (Some len, L.Load (L.I64, L.Id len_ptr)) b1 in
  let too_small = fresh "too_small" in
  let b3 = B.add_insn (Some too_small, L.Icmp (L.Slt, L.I64, i, L.IConst64 0L)) b2 in
  let too_big = fresh "too_big" in
  let b4 = B.add_insn (Some too_big, L.Icmp (L.Sge, L.I64, i, L.Id len)) b3 in
  let out = fresh "out_of_bounds" in
  let b5 = B.add_insn (Some out, L.Binop (L.Or, L.I1, L.Id too_small, L.Id too_big)) b4 in
  let err_lbl = fresh "bounds_error" in
  let ok_lbl = fresh "bounds_ok" in
  let b6 = B.term_block (L.Cbr (L.Id out, err_lbl, ok_lbl)) b5 in
  let b7 = B.start_block err_lbl b6 in
  let b8 = B.add_insn (None, L.Call (L.Void,
             L.Gid (Sym.symbol "report_error_array_index_out_of_bounds"), [])) b7 in
  let b9 = B.term_block L.Unreachable b8 in
  B.start_block ok_lbl b9



let rec trans_expr (env: env) (e: T.expr) (b: B.cfg_builder) : B.cfg_builder * L.operand = 
  match e with 
  | T.Integer {int} -> (b, L.IConst64 int)
  | T.Boolean {bool} -> (b, L.IConst1 bool)
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
         (match type_of_expr left with
          | T.RetTyp T.Str ->
              (* strings are compared by value: a op b  <=>  compare_strings(a, b) op 0 *)
              let c = fresh "cmp" in
              let b3 = B.add_insn (Some c,
                         L.Call (L.I64, L.Gid (Sym.symbol "compare_strings"), [(L.Ptr, o1); (L.Ptr, o2)])) b2 in
              let temp = fresh "temp" in
              let b4 = B.add_insn (Some temp, L.Icmp (ll_cnd_of_binop op, L.I64, L.Id c, L.IConst64 0L)) b3 in
              (b4, L.Id temp)
          | lt ->
              (* ints, bools, bytes; records, arrays and nil compare as pointers *)
              let temp = fresh "temp" in
              let b3 = B.add_insn (Some temp,
                         L.Icmp (ll_cnd_of_binop op, ll_type_of_rettyp lt, o1, o2)) b2 in
              (b3, L.Id temp))
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
         let b6 = B.add_insn (None, L.Call (L.Void, L.Gid (Sym.symbol "report_error_division_by_zero"), [])) b5 in         let b7 = B.term_block L.Unreachable b6 in
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
      | T.Lnot -> B.add_insn(Some temp, L.Binop(L.Xor, L.I1, o, L.IConst1 true)) b1
      )
      in 
      (b2, L.Id temp)
  | T.Call {fname = T.Ident {sym}; args; tp} -> 
      let (b1, ops) = trans_args env args b in 
      let callee = Sym.symbol (llvm_fun_name (Sym.name sym)) in
      let instruction = L.Call( ll_type_of_rettyp tp, L.Gid callee, ops) in 
      (match tp with
      | T.Void -> 
          let b2 = B.add_insn (None, instruction) b1 in 
          (b2, L.Null)
      | T.RetTyp _ -> 
          let temp = fresh "temp" in 
          let b2 = B.add_insn (Some temp, instruction) b1 in 
          (b2, L.Id temp)
        )
  | T.CommaExpr {left; right; _} ->
      let (b1, _) = trans_expr env left b in
      let (b2, o2) = trans_expr env right b1 in
      (b2, o2)
  | T.Lval lvl ->
      let (b1, addr, llty) = trans_lval_addr env lvl b in
      let temp = fresh "temp" in
      let b2 = B.add_insn (Some temp, L.Load (llty, addr)) b1 in
      (b2, L.Id temp)
  | T.Assignment {lvl; rhs; _} ->
      (* address first, then the value: left-to-right evaluation *)
      let (b1, addr, llty) = trans_lval_addr env lvl b in
      let (b2, o) = trans_expr env rhs b1 in
      let b3 = B.add_insn (None, L.Store (llty, o, addr)) b2 in
      (b3, o)
  | T.String {string} ->
      let g = fresh "string_literal" in
      let n = String.length string in
      let ty = L.Struct [L.I64; L.Array (n, L.I8)] in
      let init = L.GStruct [ (L.I64, L.GInt n); (L.Array (n, L.I8), L.GString string) ] in
      globals := (g, (ty, init)) :: !globals;
      (b, L.Gid g)
  | T.LengthOf {expr} ->
      let (b1, p) = trans_expr env expr b in
      (* arrays may be nil; strings never are *)
      let b2 = (match type_of_expr expr with
                | T.RetTyp (T.Array _) -> check_not_nil p b1
                | _ -> b1) in
      let len_ptr = fresh "len_ptr" in
      let b3 = B.add_insn (Some len_ptr,
                 L.Gep (L.Namedt (Sym.symbol "array_type"), p, [L.IConst32 0l; L.IConst32 0l])) b2 in
      let len = fresh "len" in
      let b4 = B.add_insn (Some len, L.Load (L.I64, L.Id len_ptr)) b3 in
      (b4, L.Id len)
  | T.Nil -> (b, L.Null)
  | T.Rcrd {recname = (T.RecordName {sym = rsym} as rn); fields} ->
      let rty = L.Namedt (record_tid rsym) in
      let (b1, size) = size_of rty b in
      let p = fresh "record" in
      let b2 = B.add_insn (Some p,
                 L.Call (L.Ptr, L.Gid (Sym.symbol "allocate_record"), [(L.I32, size)])) b1 in
      (* initialize the fields in source order (= evaluation order) *)
      let b3 = List.fold_left (fun bacc (fname, e) ->
          let (b', o) = trans_expr env e bacc in
          let (idx, ftp) = field_info rn fname in
          let fptr = fresh "field_ptr" in
          let b'' = B.add_insn (Some fptr,
                      L.Gep (rty, L.Id p, [L.IConst32 0l; L.IConst32 (Int32.of_int idx)])) b' in
          B.add_insn (None, L.Store (ll_type_of_typ ftp, o, L.Id fptr)) b'')
        b2 fields in
      (b3, L.Id p)
  | T.Arr {elemtp; len} ->
    let (b1, n) = trans_expr env len b in
    let llty = ll_type_of_typ elemtp in
    let (b2, size) = size_of llty b1 in
    (* allocate_array needs a pointer to the default element value *)
    let def = fresh "default" in
    let b3 = B.add_alloca (def, llty) b2 in
    let b4 = B.add_insn (None, L.Store (llty, default_value elemtp, L.Id def)) b3 in
    let p = fresh "array" in
    let b5 = B.add_insn (Some p,
                L.Call (L.Ptr, L.Gid (Sym.symbol "allocate_array"),
                        [(L.I32, size); (L.I64, n); (L.Ptr, L.Id def)])) b4 in
    (b5, L.Id p)
and trans_args (env: env) (args: T.expr list) (b : B.cfg_builder) : B.cfg_builder * (L.ty * L.operand) list = 
match args with 
| [] -> (b, [])
| arg :: rest -> 
  let (b1, o) = trans_expr env arg b in 
  let ty = ll_type_of_rettyp (type_of_expr arg) in 
  let (b2, rest_ops) = trans_args env rest b1 in 
  (b2, (ty, o) :: rest_ops)
(* The address (lvalue) of an lval, and the LLVM type of the value stored there. *)
and trans_lval_addr (env : env) (lvl : T.lval) (b : B.cfg_builder)
    : B.cfg_builder * L.operand * L.ty =
  match lvl with
  | T.Var {ident = T.Ident {sym}; _} ->
      let (slot, llty) = Sym.Table.find sym env in
      (b, L.Id slot, llty)
  | T.Fld {rcrd; recname = (T.RecordName {sym = rsym} as rn); field; tp} ->
      let (b1, p) = trans_expr env rcrd b in
      let b2 = check_not_nil p b1 in
      let (idx, _) = field_info rn field in
      let fptr = fresh "field_ptr" in
      let b3 = B.add_insn (Some fptr,
                 L.Gep (L.Namedt (record_tid rsym), p,
                        [L.IConst32 0l; L.IConst32 (Int32.of_int idx)])) b2 in
      (b3, L.Id fptr, ll_type_of_typ tp)
  | T.Idx {arr; index; tp} ->
      let (b1, p) = trans_expr env arr b in
      let (b2, i) = trans_expr env index b1 in
      let b3 = check_not_nil p b2 in
      let b4 = check_bounds p i b3 in
      let llty = ll_type_of_typ tp in
      let contents = fresh "contents" in
      let b5 = B.add_insn (Some contents,
                 L.Gep (L.Namedt (Sym.symbol "array_type"), p, [L.IConst32 0l; L.IConst32 1l])) b4 in
      let elem = fresh "elem_ptr" in
      let b6 = B.add_insn (Some elem, L.Gep (llty, L.Id contents, [i])) b5 in
      (b6, L.Id elem, llty)


let rec trans_stmt (env: env) (break_lbl: L.lbl) (cont_lbl: L.lbl) (s: T.statement) (b: B.cfg_builder) : env * B.cfg_builder = 
  match s with 
  | T.ReturnStm {ret = Some e} ->
    let (b1, op) = trans_expr env e b in
    let ty = ll_type_of_rettyp (type_of_expr e) in
    let b2 = B.term_block (L.Ret (ty, Some op)) b1 in
    let b3 = B.start_block (fresh "after_return") b2 in
    (env, b3)
  | T.ReturnStm {ret = None} ->
    let b1 = B.term_block (L.Ret (L.Void, None)) b in
    let b2 = B.start_block (fresh "after_return") b1 in
    (env, b2)
  | T.ExprStm {expr} -> 
      (match expr with
      | Some e -> 
        let (b1, _) = trans_expr env e b in 
        (env, b1)
      | None ->  (env, b) )
  | T.VarDeclStm (T.DeclBlock decls) ->     
    List.fold_left (fun (env_acc, b_acc) (T.Declaration {name = T.Ident {sym}; tp; body}) -> 
     let (b1, op1) = trans_expr env_acc body b_acc in 
     let ll_typ = ll_type_of_typ tp in
     let fresh_slot = fresh (Sym.name sym) in 
     let b2 = B.add_alloca (fresh_slot, ll_typ) b1 in 
     let b3 = B.add_insn (None, L.Store (ll_typ, op1, L.Id fresh_slot)) b2 in  
     let new_env = Sym.Table.add sym (fresh_slot, ll_typ) env_acc in 
     (new_env, b3)) 
     (env, b) decls
  | T.CompoundStm {stms} -> 
      let (_, b1) = 
        List.fold_left (fun (env', b') s -> trans_stmt env' break_lbl cont_lbl s b') (env,b) stms in 
      (env, b1)
  | T.IfThenElseStm {cond; thbr; elbro} -> 
      let (b1, cond_op) = trans_expr env cond b in 
      let then_lbl = fresh "then" in 
      let else_lbl = fresh "else" in 
      let merge_lbl = fresh "merge" in 
      let b2 = B.term_block (L.Cbr (cond_op, then_lbl, else_lbl)) b1 in 
      (* then branch *)
      let b3 = B.start_block then_lbl b2 in 
      let (_, b4) = trans_stmt env break_lbl cont_lbl thbr b3 in 
      let b5 = B.term_block (L.Br merge_lbl) b4 in 
      (*else branch *)
      let b6 = B.start_block else_lbl b5 in 
      let b7 = 
        (match elbro with 
        | Some elbr -> let (_,b') = trans_stmt env break_lbl cont_lbl elbr b6 in b'
        | None -> b6) in
      let b8 = B.term_block (L.Br merge_lbl) b7 in 
      let b9 = B.start_block merge_lbl b8 in 
      (env, b9) 
    | T.WhileStm {cond; body} -> 
      let cond_lbl = fresh "loop_cond" in 
      let body_lbl = fresh "loop_body" in
      let end_lbl = fresh "loop_end" in
      let b1 = B.term_block (L.Br cond_lbl) b in
      let b2 = B.start_block cond_lbl b1 in
      let (b3, cond_op) = trans_expr env cond b2 in     
      let b4 = B.term_block (L.Cbr (cond_op, body_lbl, end_lbl)) b3 in
      let b5 = B.start_block body_lbl b4 in     
      let (_, b6) = trans_stmt env end_lbl cond_lbl body b5 in     
      let b7 = B.term_block (L.Br cond_lbl) b6 in
      let b8 = B.start_block end_lbl b7 in
      (env, b8)
    | T.ForStm {cond; init; update; body} -> 
      let cond_lbl   = fresh "for_cond" in     
      let body_lbl   = fresh "for_body" in     
      let update_lbl = fresh "for_update" in
      let end_lbl    = fresh "for_end" in 
      let (env1, b1) = (match init with    
      | Some (T.FIDecl db) -> trans_stmt env break_lbl cont_lbl (T.VarDeclStm db) b    
      | Some (T.FIExpr e)  -> let (b', _) = trans_expr env e b in (env, b')    
      | None -> (env, b)) in   
      let b2 = B.term_block (L.Br cond_lbl) b1 in
      let b3 = B.start_block cond_lbl b2 in     
      let b5 = (match cond with 
      | Some c -> 
        let (b4, cond_op) = trans_expr env1 c b3 in 
        B.term_block (L.Cbr (cond_op, body_lbl, end_lbl)) b4 
        | None -> B.term_block (L.Br body_lbl) b3) in
        let b6 = B.start_block body_lbl b5 in
        let (_, b7) = trans_stmt env1 end_lbl update_lbl body b6 in
        let b8 = B.term_block (L.Br update_lbl) b7 in
        let b9 = B.start_block update_lbl b8 in 
        let b11 =  
          (match update with 
          | Some u -> let (b10, _) = trans_expr env1 u b9 in b10 
          | None -> b9) in 
          let b12 = B.term_block (L.Br cond_lbl) b11 in 
          let b13 = B.start_block end_lbl b12 in 
          (env, b13)
    | T.BreakStm -> let b1 = B.term_block (L.Br break_lbl) b in     
          let b2 = B.start_block (fresh "after_break") b1 in 
          (env, b2)
    | T.ContinueStm -> 
      let b1 = B.term_block (L.Br cont_lbl) b in     
      let b2 = B.start_block (fresh "after_continue") b1 in     
      (env, b2)

let trans_fundecl ({ret; funname = T.Ident {sym}; params; body} : T.fundecl) : Sym.symbol * L.fdecl =
  let fn_name = llvm_fun_name (Sym.name sym) in
  let fn_gid = Sym.symbol fn_name in
  let param_tys = List.map (fun (T.Param {typ; _}) -> ll_type_of_typ typ) params in
  let ret_llty = ll_type_of_rettyp ret in
  let fty = (param_tys, ret_llty) in
  (* Generate fresh uids for LLVM parameters *)
  let param_uids = List.map (fun (T.Param {paramname = T.Ident {sym = psym}; _}) ->
    fresh (Sym.name psym ^ "_arg")
  ) params in
  (* Allocate local stack space for copying each argument *)
  let (env, b_params) =
    List.fold_left (fun (env_acc, b_acc) (T.Param {paramname = T.Ident {sym = psym}; typ}, arg_uid) ->
      let llty = ll_type_of_typ typ in
      let local_slot = fresh (Sym.name psym ^ "_local_arg_copy") in
      let b1 = B.add_alloca (local_slot, llty) b_acc in
      let b2 = B.add_insn (None, L.Store (llty, L.Id arg_uid, L.Id local_slot)) b1 in
      let env' = Sym.Table.add psym (local_slot, llty) env_acc in
      (env', b2)
    ) (empty_env, B.empty_cfg_builder) (List.combine params param_uids)
  in
  let no_break = fresh "no_break" in
  let no_cont = fresh "no_cont" in
  let (_, b_body) =
    List.fold_left (fun (env_acc, b_acc) s -> trans_stmt env_acc no_break no_cont s b_acc)
      (env, b_params) body
  in
  (* Terminate the block if needed *)
  let final_b =
    if B.is_labelled b_body then
      match ret with
      | T.Void -> B.term_block (L.Ret (L.Void, None)) b_body
      | T.RetTyp _ -> B.term_block L.Unreachable b_body
    else
      b_body
  in
  let cfg = B.get_cfg final_b in
  (fn_gid, { L.fty; param = param_uids; cfg })

let codegen_prog (prog: T.program) : L.prog =
  globals := [];
  Hashtbl.reset records;
  let recdecls = List.filter_map (function T.RecDecl r -> Some r | T.FunDecl _ -> None) prog in
  List.iter (fun ({recname = T.RecordName {sym}; fields} : T.recdecl) ->
      Hashtbl.replace records (Sym.name sym)
        (List.map (fun (T.FieldName {sym = f}, tp) -> (Sym.name f, tp)) fields))
    recdecls;
  let record_tdecls = List.map (fun ({recname = T.RecordName {sym}; fields} : T.recdecl) ->
      (record_tid sym, L.Struct (List.map (fun (_, tp) -> ll_type_of_typ tp) fields)))
    recdecls in
  let fdecls = List.filter_map (function
    | T.FunDecl f -> Some (trans_fundecl f)
    | T.RecDecl _ -> None) prog in 
    { L.tdecls = (Sym.symbol "dolphin_record_stream", L.Struct [])
               :: (Sym.symbol "array_type", L.Struct [L.I64; L.Array (0, L.I8)])
               :: record_tdecls;
      extgdecls = [ (Sym.symbol "dolphin_rc_empty_string", L.Namedt (Sym.symbol "array_type")) ];
      gdecls = List.rev !globals;
      extfuns = List.map (fun (name, fty) -> (Sym.symbol name, fty)) external_functions;
      fdecls;
    }
