{
  (* default Quarklang source code extension *)
  let extension = ".qk";;
}

let white = [' ' '\t' '\r' '\n']  
let nonwhite = [^ ' ' '\t' '\r' '\n' ';']  

(* code: string,  imports: string[] of file paths *)
rule process code imports = parse
    (* we shouldn't preprocess anything in a string literal *)
  | ('"' ('\\' _ | [^ '"'])* '"') as s { process (code ^ s) imports lexbuf }
    (* gets the import file name *)
  | "import" white* ((nonwhite [^';']* nonwhite) as filename) white* ';' { 
      let import_file = filename ^ extension in
      process code (import_file :: imports) lexbuf
    }
  | "import" white* ';'  { failwith "Empty import statement" }
    (* supports python-style elif by simple string replacement!  *)
  | "elif"  { process (code ^ "else if") imports lexbuf }
    (* copy anything else verbatim *)
  | _ as c  { process (code ^ Char.escaped c) imports lexbuf }
    (* returns both the processed code and list of imported files *)
  | eof  { (code, List.rev imports) }