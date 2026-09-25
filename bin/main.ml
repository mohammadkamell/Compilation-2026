(* -- Use this in your solution without modifications *)

(** Main module: this is where the program starts when you execute it in the CLI.
    It uses the library `Cmdliner` to parse the command of the CLI.
    It is an interface between the CLI and the `Compiler` module that you have to implement as part
    of the assignments.
*)

(** The compiler supports 2 modes: `compile` and `rescue`.
    `compile`: For phases 1 and 2, it is essentially equivalent to `rescue` mode.
              For phases >= 3, it uses your full end-to-end compiler, including your own parser.
    `rescue`: Compile a Dolphin source, but uses an external serializer to parse the source file
                into an AST
    /!\ Only use the `rescue` mode for testing. You are graded using the `compile` mode. /!\
*)


(* Default command: just display the help. *)
let default_info =
  let open Cmdliner in
  let doc = "Dolphin implementation" in
  let man = [
    `S Manpage.s_common_options;
    `P "These options are common to all commands.";
    `S Manpage.s_bugs;
    `P "Report bugs on ..." ]
  in
  Cmd.info "dolphin" ~version:"dolphin 1" ~doc ~man
let default_cmd = Cmdliner.Term.(ret (const (`Help (`Plain, None))))


(* -- Variables initialised by the CLI -- *)

(* Current phase of the compiler. It is necessary to know, to be able to call the appropriate *)
(* serializing phase in the --rescue mode. *)
let phase =
  let open Cmdliner in
  let doc = "The phase $(docv) of dolphin is enabled. The default value is Phase 5 dolphin." in
  Arg.(value & opt (enum [("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5)]) 5 & info ["phase"] ~docv:"phase" ~doc)

(* Describes whether the input in a Dolphin source or a serialized AST. *)
let from_ast =
  let open Cmdliner in
  let doc = "If activated, expects a serialized AST as input." in
  Arg.(value & flag & info ["from-ast"] ~doc)

(* Output filepath of the compiler *)
let outfile =
  let open Cmdliner in
  let doc = "$(docv) is the file to write to. Use $(b,-) for $(b,stdout)" in
  Arg.(value & opt string "-" & info ["o"; "output-file"] ~doc ~docv:"FILE")



(* --- Helper function to handle exceptions --- *)
let check f x =
  try f x with
  | e -> Printf.printf "Error: %s\n" (Printexc.to_string e); exit 1

let make_sure_exactly_one_file_provided files =
  if List.length files != 1 then
  begin
    Printf.eprintf "There should be exactly one file given but %d files are provided.\n"
      (List.length files) ;
    exit 1
  end

(* --- Functions to execute the different modes of the compiler --- *)
(* --- This is here that we link the `Compiler` module to `main` --- *)

(* Run the external serializer. *)
let run_serialize phase filepath =

  (* Create a temporary file *)
  let filename = Filename.basename filepath in
  let (tmp_json, tmp_json_out_channel) = check Filename.open_temp_file filename "" in
  (* Clean up the file after we're done *)
  at_exit (fun () -> Sys.remove tmp_json);

  (* Command to execute *)
  let dlp_to_json = "dolphin-serialize serialize" in
  let cmd_dlp_to_json = Printf.sprintf "%s --phase %d --output %s %s 2>&1" dlp_to_json phase tmp_json filepath in
  let res = Sys.command cmd_dlp_to_json in
  close_out tmp_json_out_channel;
  (res, tmp_json)


let llvm_prog_to_channel output_channel llvm_prog =
    Printf.fprintf output_channel "%s" (Ll.string_of_prog llvm_prog)

(* Outputs the generated LLVM program in `output_filepath`. *)
(* If the `ouput_filepath` is empty, outputs on the standard output. *)
let llvm_to_out llvm_prog filepath =
  if filepath = "-"
  then llvm_prog_to_channel stdout llvm_prog
  else
    try
      let output_channel = open_out filepath in
      llvm_prog_to_channel output_channel llvm_prog;
      close_out output_channel
    with _ ->
      failwith ("Something went wrong while trying to open the file \"" ^ filepath ^ "\" for reading.\n")


(* Execute the compiler from a serialized AST. *)
let compile_from_json serialized_ast output_filepath =
  (* `serialized_ast` needs to be deserialized, which returns an AST *)
  let prog_ast = Deserializer.program_of_yojson serialized_ast in
  (* ----- /!!\ The compiler function `compile_prog_from_ast` is executed here. /!!\ ----- *)
  let llvm_prog_opt = Compiler.compile_prog_from_ast prog_ast in
  match llvm_prog_opt with
  | None ->
    (* The compiler did not go through. *)
    exit 1
  | Some llvm_prog ->
    (* Outputs the generated LLVM program in `output_filepath`. *)
    llvm_to_out llvm_prog output_filepath; exit 0
    (* Outputs the generated LLVM program in `output_filepath`. *)

(* Execution with the `--from-ast`. It expects a path to a serialized_ast  *)
let run_from_ast filepaths output_filepath =
  (* Get the Dolphin source file from the path given in argument of the CLI *)
  make_sure_exactly_one_file_provided filepaths ;
  let filepath = List.hd filepaths in
  (* Execute the compiler from the serialized AST. *)
  compile_from_json (Yojson.Safe.from_file filepath) output_filepath

(* Execution of the `normal` mode. It expects a path to a Dolphin source. *)
let run_full_compiler filepaths output_filepath =
  (* Get the Dolphin source file from the path given in argument of the CLI *)
  make_sure_exactly_one_file_provided filepaths ;
  let filepath = List.hd filepaths in
  (* ----- /!!\ The compiler function `compile_prog_from_filename` is executed here. /!!\ ----- *)
  let llvm_prog_opt = Compiler.compile_prog_from_filename filepath in
  match llvm_prog_opt with
  | None ->
    (* The compiler did not go through. *)
    exit 1
  | Some llvm_prog ->
    (* Outputs the generated LLVM program in `output_filepath`. *)
    llvm_to_out llvm_prog output_filepath; exit 0

(* Execution of the `rescue` mode, which uses the serializer to create a serialized AST *)
(* from the input source file. *)
let run_rescue_mode phase filepaths output_filepath =
  (* Get the Dolphin source file from the path given in argument of the CLI *)
  make_sure_exactly_one_file_provided filepaths ;
  let filepath = List.hd filepaths in

  (* Execute the external serializer. If successful, it generates a serialized AST in the file `json_path`. *)
  let (serialize_res, json_path) = run_serialize phase filepath in
  if serialize_res <> 0
  then failwith "Something went wrong with the serializer"
  (* If successful, execute the compiler from the serialized AST. *)
  else compile_from_json (Yojson.Safe.from_file json_path) output_filepath

(* If the compiler executes with the normal mode, *)
(* it can either bypass the parsing phase with the option `--from_ast`, *)
(* or is equivalent `rescue` mode for phases 1 and 2. *)
let run_compile from_ast phase filepaths output_filepath =
  if from_ast
  then run_from_ast filepaths output_filepath
  else
    (if phase < 3
     then run_rescue_mode phase filepaths output_filepath
     else run_full_compiler filepaths output_filepath
    )

(* --- Parsing of the CLI commands with Cmdliner  --- *)

(* Parse the CLI command in the case of the rescue mode *)
let cmd_rescue_mode =
  let open Cmdliner in
  let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILE") in
  let doc =
    "Runs the Dolphin compiler from a Dolphin source, but uses the reference parser." in
  let exits = Cmd.Exit.defaults in
  let man =
    [`S Manpage.s_description;
      `P
        "This command executes the Dolphin's compiler from a Dolphin source file using the reference
   parser. Requires `dolphin-serialize` to be in the PATH.";]
  in
  Cmd.v
    (* Cmd.info defines a new command, here the CLI command is "rescue" *)
    (Cmd.info "rescue" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)
    (* If the `rescue` command is detected, CmdLiner parses *)
    (* the possible options, and executes the function `run_rescue_mode` with the parsed options *)
    Term.(const run_rescue_mode $ phase $ files $ outfile)

(* Parse the CLI command in the case of the normal mode *)
let cmd_compile =
  let open Cmdliner in
  let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILE") in
  let doc =
    "Runs the Dolphin compiler from a Dolphin source." in
  let exits = Cmd.Exit.defaults in
  let man =
    [`S Manpage.s_description;
      `P "This command executes the Dolphin's compiler from a Dolphin source file.
        Before phase 3, uses the reference parser. After phase 3, uses the student's parser.";]
  in
  Cmd.v
    (* Cmd.info defines a new command, here the CLI command is "compile" *)
    (Cmd.info "compile" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)
    (* If the `compile` command is detected, CmdLiner parses *)
    (* the possible options, and executes the function `run_compile` with the parsed options *)
    Term.(const run_compile $ from_ast $ phase $ files $ outfile)


(* The compiler supports 2 modes: normal and rescue. *)
let cmds = [
    cmd_compile; (* Normal mode: In phases 1 and 2, equivalent to Rescue mode. *)
                 (* In phases 3 and more, compiles a Dolphin source file with your own parser. *)
                 (* This mode also accepts an option `--from-ast` to bypass the parser entirely. *)
    cmd_rescue_mode (* Rescue mode: Compiles from a Dolphin source file, but uses the external *)
                    (* serializer to parse the source file. *)
]


(* Boilerplate for Cmdliner: parse the CLI with the possible commands described in `cmds` *)
let main () = exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.group ~default:default_cmd default_info cmds))
(* The entry point of the program is here. *)
let _ = main ()
