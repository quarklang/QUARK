type action = Raw | Ast | Interpret | Bytecode | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-r", Raw);
                              ("-a", Ast);
			      ("-i", Interpret);
			      ("-b", Bytecode);
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Raw -> print_string (Ast.program_s program)
  | Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Interpret -> Interpret.run program
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing
  | Compile -> Execute.execute_prog (Compile.translate program)
 
