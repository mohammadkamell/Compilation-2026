(* Test that Parser and Lexer work together on all test files *)

let parse_file filename =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf filename;
  Fun.protect
    ~finally:(fun () -> close_in ch)
    (fun () -> Parser.program Lexer.token lexbuf)

let test_dir dir =
  let files = Sys.readdir dir in
  Array.sort String.compare files;
  let passed = ref 0 in
  let failed = ref 0 in
  Array.iter (fun file ->
    if Filename.check_suffix file ".dlp" then begin
      let path = Filename.concat dir file in
      try
        let ast = parse_file path in
        assert (List.length ast >= 0);
        incr passed;
        Printf.printf "  [OK] %s (parsed %d statements)\n" file (List.length ast)
      with
      | Lexer.Error (loc, msg) ->
        incr failed;
        Printf.eprintf "  [LEXER ERROR] %s: %s (%s)\n"
          file msg (PrintBox_text.to_string (Location.location_to_tree loc))
      | Parser.Error ->
        incr failed;
        Printf.eprintf "  [PARSER ERROR] %s: syntax error\n" file
      | exn ->
        incr failed;
        Printf.eprintf "  [ERROR] %s: %s\n" file (Printexc.to_string exn)
    end
  ) files;
  (!passed, !failed)

let () =
  Printf.printf "=== Testing Positive Tests ===\n";
  let pos_p, pos_f = test_dir "test/tests/positive" in
  Printf.printf "Positive: %d passed, %d failed\n\n" pos_p pos_f;

  Printf.printf "=== Testing Negative Tests (Syntax Check) ===\n";
  let neg_p, neg_f = test_dir "test/tests/negative" in
  Printf.printf "Negative (syntax): %d passed, %d failed\n\n" neg_p neg_f;

  if pos_f > 0 || neg_f > 0 then exit 1
  else Printf.printf "=== All %d files successfully parsed by Lexer + Parser! ===\n" (pos_p + neg_p)
