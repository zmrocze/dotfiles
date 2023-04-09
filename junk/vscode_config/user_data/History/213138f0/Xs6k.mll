{
  (* Standardowo w YACC-podobnych narzędziach to lekser jest uzależniony od parsera.
   * To znaczy, że typ danych z tokenami definiuje moduł wygenerowany na bazie parser.mly.
   *)
  open Zi_lib
  open Parser
  open Parser_utils

  (* Lexing z biblioteki standardowej ocamla *)
  open Lexing

  (* Definiujemy alias na typ tokenu na potrzeby interfejsów Zi_lib.Iface *)
  type token = Parser.token

  (* Obsługa błędu *)
  let handleError lexbuf =
      let pos = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
      let token = Lexing.lexeme lexbuf in
      let exc = InvalidToken (mkLocation pos, token) in
      raise exc
      
  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   * Miejsce na twój kod w Ocamlu
   *)
  open String
  (* let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl *)

  (* let keyword_table = create_hashtable 20  [
    ("and" , AND) ;
    ("bool" , BOOL) ;
    ("else" , ELSE) ;
    ("false" , FALSE) ;
    ("if" , IF) ;
    ("int" , INT) ;
    ("length" , LENGTH) ;
    ("not" , NOT) ;
    ("or" , OR) ;
    ("return" , RETURN) ;
    ("true" , TRUE) ;
    ("type" , TYPE) ;
    ("use" , USE) ;
    ("while" , WHILE)
  ] *)
  (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ----------------------------------------------------------------------------- *)

  }
  
  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   * Miejsce na nazwane wyrażenia regularne
   *)
  let printable = [' '-'~']
  let identifier    = ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' ''']*
  let printable_char = '''(printable # '\\')'''
  let integer = ['0'-'9']+
  (* let alpha = [' ' '!' '#'-'[' ']'-] *)
  let string = '"'((printable # ['\' '"']) )*'"'
  (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ----------------------------------------------------------------------------- *)


  rule token = parse
      (* Trzeba pamiętać aby uaktualnić pozycje w lexbuf, gdy widzimy znak końca wiersza.
       * To się samo nie robi. Moduł Lexing z standardowej biblioteki daje do tego wygodną
       * funkcję new_line.
       *)
      | ['\n']
      { new_line lexbuf; token lexbuf }

      (* widzimy początek komentarza i przechodzimy do pomocniczego stanu *)
      | "//"
      { line_comment lexbuf }

      | eof
      { EOF }

      (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
       * Miejsce na twoje reguły
       *)

      (* keywords *)
      | "and" { AND }
      | "bool" { BOOL }
      | "else" { ELSE }
      | "false" { FALSE }
      | "if" { IF }
      | "int" { INT }
      | "length" { LENGTH }
      | "not" { NOT }
      | "or" { OR }
      | "return" { RETURN }
      | "true" { TRUE }
      | "type" { TYPE }
      | "use" { USE }
      | "while" { WHILE }

      | '{' { BRACE_CURLY_LEFT }
      | '}' { BRACE_CURLY_RIGHT }
      | '[' { BRACE_SQUARE_LEFT }
      | ']' { BRACE_SQUARE_RIGHT }
      | '(' { BRACE_LEFT }
      | ')' { BRACE_RIGHT }
      | ':' { COLON }
      | ',' { COMMA }
      | '.' { DOT }
      | ';' { SEMICOLON }
      | '^' { UPPER } 
      | "==" { EQ }
      | "!=" { NEQ }
      | "<" { LT }
      | '>' { GT }
      | "<=" { LE }
      | ">=" { GE }
      | '=' { EQUALS }
      | '-' { MINUS }
      | '+' { PLUS }
      | '*' { TIMES }
      | '/' { DIV }
      | '%' { MOD }

      | identifier as id { failwith id }

      (* char literal *)
      (* | printable_char as c { CHAR c.[1] } *)
      | ''' { CHAR @@ 
        match character lexbuf with
          | Some c -> closing_tick lexbuf ; c 
          | None -> handleError lexbuf
        }

      (* | "'\\0'" {CHAR '\NUL'}
      | "'\\n'" {CHAR '\n'}
      | "'\\t'" {CHAR '\t'}
      | "'\\r'" {CHAR '\r'}
      | "'\\\"'" {CHAR '"'}
      | "'\\''" {CHAR '''}
      | "'\\\\'" {CHAR '\'} *)

      (* int literal *)
      | integer as n { INTEGER (string_of_int n) }
      
      (* bad: integer followed by a thing *)
      | integer['a'-'z' 'A'-'Z' ''' '_'] 
        { handleError lexbuf }

      (* string literal *)
      | '"' {
        let takeUntil f lexbuf str = match f lexbuf with
          | None -> str
          | Some c -> takeUntil f lexbuf (cat str (String.make 1 c)) 
        in String (takeUntil character lexbuf String.empty) }

      (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         ----------------------------------------------------------------------------- *)
  
      | _
      { handleError lexbuf }

  and character = parse
      | '"' { None }
      | "\\0" { Some '\NUL' }
      | "\\n" { Some '\n' }
      | "\\t" { Some '\t' }
      | "\\r" { Some '\r' }
      | "\\\"" { Some  '"' }
      | "\\'" { Some ''' }
      | "\\\\" { Some  '\' }
      | (printable # '\') as c { Some (c.[0]) }
      | _ { handleError lexbuf }

  (* helper to consume closing tick after char literal *)
  and closing_tick = parse 
      | ''' { () }
      | _ { handleError } 
  
  (* Pomocniczy stan aby wygodnie i prawidłowo obsłużyć komentarze *)
  and line_comment = parse
      | '\n' 
      { new_line lexbuf; token lexbuf }

      (* Niektóre edytory nie wstawiają znaku końca wiersza w ostatniej linijce, jesteśmy
       * przygotowani na obsługę takiego komentarza.
       *)
      | eof
      { EOF }

      | _ 
      { line_comment lexbuf }
