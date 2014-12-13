open Semantic

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let _ =

  let lexbuf = Lexing.from_string "elif shit; import    asdf  \"import\"    ; " in
  let processed_code, imports = Preprocessor.process "" [] lexbuf in
  let imports = List.fold_left (fun acc x -> acc ^x^ "; ") "" imports in
  print_endline @@ processed_code ^"\nimported: "^ imports
  
  (*
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.top_level Scanner.token lexbuf in
  let env = { 
    var_table = StrMap.empty; 
    func_table = StrMap.empty;
    func_current = "";
    depth = 0;
    is_returned = true;
    in_loop = false;
  }
  in
  let _, sast = Semantic.gen_sast env ast in
  let code = Generator.gen_code sast in
  let code = Generator.header_code ^ code in
  let _ = print_endline code in
  output_string (open_out "output.cpp") code
  *)