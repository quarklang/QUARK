open Semantic

let _ =

  print_endline @@ Preprocessor.process "../what.qk"
  (*
  let lexbuf = Lexing.from_string "elif shit; import     \t\t  asdf  \"import\"; import elif asdf; elif import shit     ; " in
  let processed_code, imports = Preprocessor.process "" [] lexbuf in
  let importss = List.fold_left (fun acc x -> acc ^x^ ":: ") "" imports in
  let _ = 
    print_endline @@ processed_code ^"\nimported("^string_of_int (List.length imports)^ "): "^ importss in
  print_endline @@ Preprocessor.read_file_str "../what.qk"
  
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