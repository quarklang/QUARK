{
  (* default Quarklang source code extension *)
  let extension = ".qk";;
}

let white = [' ' '\t' '\r' '\n']  
let nonwhite = [^ ' ' '\t' '\r' '\n' ';']  

(* code: string,  imports: string[] of file paths *)
rule scan code imports = parse
    (* we shouldn't preprocess anything in a string literal *)
  | ('"' ('\\' _ | [^ '"'])* '"') as s { scan (code ^ s) imports lexbuf }
    (* gets the import file name *)
  | "import" white* ((nonwhite [^';']* nonwhite) as filename) white* ';' { 
      let import_file = filename ^ extension in
      scan code (import_file :: imports) lexbuf
    }
  | "import" white* ';'  { failwith "Empty import statement" }
    (* supports python-style elif by simple string replacement!  *)
  | white+ "elif" white+ { scan (code ^ " else if ") imports lexbuf }
    (* copy anything else verbatim *)
  | _ as c  { scan (code ^ String.make 1 c) imports lexbuf }
    (* returns both the processed code and list of imported files *)
  | eof  { (code, List.rev imports) }
  
(* trailer *)
{
let read_file_lines filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines
  

let read_file_str filename = 
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ "\n" ^ input_line chan
    done; ""
  with End_of_file ->
    close_in chan;
    !lines

(* Handle nested imports and circular imports *)
module StrSet = Set.Make(String)

(* shaded scan *)
let scan code =
  scan "" [] (Lexing.from_string code)

(* preprocessor gets the import relative to the source file itself *)
(* not where the user invokes the quark compiler *)
let process filename = 
  let filedir = Filename.dirname filename in
  (* return processed_code, new_seen_set *)
  let rec proc_rec filename seen_set =
    try
      let this_code, imports =
        scan (read_file_str filename) in
      let imported_code, seen_set' = 
        List.fold_left (
          fun (code, seen) import_file_base -> 
            let import_file = Filename.concat filedir import_file_base in
            if filename = import_file then
              failwith @@ "Circular import the source itself: " ^ import_file_base
            else if StrSet.mem import_file seen then
              (* repeated import. Don't do anything *)
              code, seen
            else
              let seen' = StrSet.add import_file seen in
              let imported_code, imported_seen = proc_rec import_file seen' in
                (* use Quark commented line as separator, for visual debugging *)
                code ^ imported_code ^ "\n% ************ imported: "
                ^ import_file_base ^" ************ %\n",
                imported_seen
        ) ("", seen_set) imports
      in
      (* imported code inserts before the original code *)
      imported_code ^ this_code, seen_set'
    with Sys_error(_) -> 
      failwith @@ "Import error: " ^ filename
  in
  (* start the recursion and discard seen_set *)
  let processed_code, _ = proc_rec filename (StrSet.singleton filename)
  in processed_code
   
}