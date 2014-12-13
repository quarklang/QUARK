open Semantic

let _ =
  let file = "../what.qk" in
  let processed_code = Preprocessor.process file in
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf = Lexing.from_string processed_code in
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