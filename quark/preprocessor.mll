{
  (* default Quarklang source code extension *)
  let extension = ".qk";;
}

let white = [' ' '\t' '\r' '\n']  
let nonwhite = [^' ' '\t' '\r' '\n']  

(* code: string,  imports: string[] of file paths *)
rule process code imports = parse
    (* we shouldn't preprocess anything in a string literal *)
  | ('"' ('\\' _ | [^ '"'])* '"') as s { process (code ^ s) imports lexbuf }
  | "import"  { 
      let import_file = (get_import "" lexbuf) ^ extension in
      process code (import_file :: imports) lexbuf
    }
    (* supports python-style elif by simple string replacement!  *)
  | "elif"  { process (code ^ "else if") imports lexbuf }
    (* copy anything else verbatim *)
  | _ as c  { process (code ^ Char.escaped c) imports lexbuf }
    (* returns both the processed code and list of imported files *)
  | eof  { (code, List.rev imports) }

(* gets the import file name *)
and get_import filename = parse
  | white* ((_* nonwhite) as filename) white* ';'  { filename }
  (*| ';'  { filename }
  | _ as c  { get_import (filename ^ Char.escaped c) lexbuf } *)
  | eof  { failwith "import statement must be terminated by ;" }