(* a record for representing the construction of the CFG *)

type cfg_builder = {
  bb_rev : (Ll.lbl * Ll.block) list;
      (* reversed list of all but the first basic blocks *)
  insn_rev : (Ll.uid option * Ll.insn) list;
      (* reversed list of the instructions that have not yet been
         allocated a basic block *)
  bb_first : Ll.block option (* first basic block, if any*);
  lbl_current : Ll.lbl option (* label of the current basic block*);
  allocas_rev : (Ll.uid * Ll.ty) list (* reserved alloca's *);
}

(* this exception is thrown if the module is used incorrectly *)
exception CFGConstructionError of string

(* --- Aauxiliary functions for accessing and manipulating the CFG -- *)

(* add a reserved alloca to the first basic block *)
let add_alloca (uid, ty) (c : cfg_builder) =
  { c with allocas_rev = (uid, ty) :: c.allocas_rev }

let is_labelled (c : cfg_builder) =
  Option.is_none c.bb_first || Option.is_some c.lbl_current

(* add an instruction to the current list of instructions *)
let add_insn (insn_ : Ll.uid option * Ll.insn) (c : cfg_builder) =
  if is_labelled c then { c with insn_rev = insn_ :: c.insn_rev }
  else
    raise
      (CFGConstructionError
         "start a new block with 'start_block' before emitting instructions")

(* terminate the current block *)
let term_block (tr : Ll.terminator) (cfg_ : cfg_builder) =
  let (bb : Ll.block) = { insns = List.rev cfg_.insn_rev; terminator = tr } in
  match cfg_.bb_first with
  | None -> { cfg_ with insn_rev = []; bb_first = Some bb; lbl_current = None }
  | Some _ -> (
      match cfg_.lbl_current with
      | Some lb ->
          {
            cfg_ with
            bb_rev = (lb, bb) :: cfg_.bb_rev;
            insn_rev = [];
            lbl_current = None;
          }
      | _ ->
          raise
            (CFGConstructionError
               "start a new block with 'start_block' before terminating"))

(* start a new block with the given label *)
let start_block (lbl : Ll.lbl) (c : cfg_builder) =
  if is_labelled c then
    raise
      (CFGConstructionError
         "terminate the current block before starting a new one")
  else { c with lbl_current = Some lbl }

(* empty CFG environment *)
let empty_cfg_builder =
  {
    bb_rev = [];
    insn_rev = [];
    bb_first = None;
    lbl_current = None;
    allocas_rev = [];
  }

(* get a CFG from the builder *)
let get_cfg (cfg_builder : cfg_builder) : Ll.cfg =
  match cfg_builder.bb_first with
  | Some { Ll.insns; Ll.terminator } ->
      let allocas =
        List.map
          (fun (uid, ty) -> (Some uid, Ll.Alloca ty))
          cfg_builder.allocas_rev
      in
      let b' = { Ll.insns = allocas @ insns; terminator } in
      (b', List.rev cfg_builder.bb_rev)
  | _ -> raise (CFGConstructionError "basic block is not available")

let get_lbl (cfg_builder: cfg_builder) : Ll.lbl option =
  cfg_builder.lbl_current

(* --- buildlets ---------------------------------------------- *)

type buildlet = cfg_builder -> cfg_builder

let id_buildlet st = st (* identity function *)

let seq_buildlets (bs : buildlet list) : buildlet =
 fun cg -> List.fold_left (fun cg' b -> b cg') cg bs
