open Semantic

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.top_level Scanner.token lexbuf in
  let env = { 
    var_table = StrMap.empty; 
    func_table = StrMap.empty;
    func_current = "";
    depth = 0;
    is_returned = true;
  }
  in
  (*TODO 
  let sast = Semantic.check_program ast in
  let c_sast = gen_pretty_c sast in
  let code = gen_program c_sast in
  let outfile = open_out "output.cpp" in
  output_string outfile code
  *)
  gen_sast env ast