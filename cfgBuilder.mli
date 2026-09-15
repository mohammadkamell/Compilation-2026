(* -- Use this in your solution without modifications *)

exception CFGConstructionError of string

type cfg_builder

type buildlet = cfg_builder -> cfg_builder
val id_buildlet: buildlet
val seq_buildlets: buildlet list -> buildlet

val is_labelled : cfg_builder -> bool

val add_alloca:  Ll.uid * Ll.ty -> buildlet
val add_insn: Ll.uid option * Ll.insn -> buildlet
val term_block: Ll.terminator -> buildlet
val start_block: Ll.lbl -> buildlet

val empty_cfg_builder: cfg_builder

val get_lbl: cfg_builder -> Ll.lbl option
val get_cfg: cfg_builder -> Ll.cfg
