(*
 * Menhir wygeneruje funkcje o nazwach source_file oraz interface_file 
 *)
%start <Zi_lib.Ast.module_definition> source_file
%start <Zi_lib.Ast.module_interface> interface_file

%{
open Zi_lib
open Ast
open Parser_utils

(* Generator znaczników *)
let mk_node loc data =
  { loc  = mkLocation loc
  ; tag  = fresh_node_tag ()
  ; data = data
  }

  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   * Miejsce na twój kod w Ocamlu
   *)
  let fmap_node f { loc; tag; data } = { loc; tag; data = f data }
  (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)
  let expr_of_lvalue v = match v with
      | LVALUE_Id id -> EXPR_Id id 
      | LVALUE_Index { expr ; index } -> EXPR_Index { expr ; index }
      | LVALUE_Field { expr ; field } -> EXPR_Field { expr ; field }

%}

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na dyrektywy
 *)

%token EOF
%token <int32> INTEGER
%token <string> IDENTIFIER
%token <string> STRING
%token <char> CHAR
// %token <bool> BOOL
%token BRACE_CURLY_LEFT
%token BRACE_CURLY_RIGHT
%token BRACE_SQUARE_LEFT
%token BRACE_SQUARE_RIGHT
%token BRACE_SQUARE_BOTH
%token BRACE_LEFT
%token BRACE_RIGHT
%token COLON
%token COMMA
%token DOT
%token UPPER
%token SEMICOLON
%token EQ
%token NEQ
%token LT
%token GT
%token LE
%token GE
%token EQUALS // assignment
(* keywords *)
%token AND
%token BOOL
%token ELSE
%token FALSE
%token IF
%token INT
%token LENGTH
%token NOT
%token OR
%token RETURN
%token TRUE
%token TYPE
%token USE
%token WHILE
(* arithmetics *) 
%token MINUS
%token PLUS
%token TIMES
%token DIV
%token MOD

%right ELSE

%left OR
%left AND
%right NOT
%left EQ NEQ
%left LE LT GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%right MINUS_UNOP
%left UPPER
%left DOT BRACE_SQUARE_LEFT BRACE_SQUARE_BOTH
// %left FUNC_CALL ARR_INDEX FIELD_SELECT UNFOLDING

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)

%%

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na gramatykę
 *)

parse_optionaly_ended_separeted_list(sep, parse):
  | { [] }
  | x = parse ; sep ; xs = parse_optionaly_ended_separeted_list(sep, parse) 
    { x :: xs }
  | x = parse ; 
    { [x] }

parse_optionaly_ended_nonempty_separeted_list(sep, parse):
  | x = parse ; sep ; xs = parse_optionaly_ended_separeted_list(sep, parse) 
    { x :: xs }
  | x = parse ; 
    { [x] }



parse_lvalue:
  | id = identifier { LVALUE_Id id }
  | x = parse_lvalue_aux(parse_expression_legal_in_lvalue) {x}

parse_lvalue_aux(parse):
  // | id = identifier { LVALUE_Id id } -- can't be here if we'd want exclusive parse_lvalue_aux(parse) for exclusive parse's
  | arr = parse_node(parse) ; BRACE_SQUARE_LEFT ; ind= parse_node(parse_expression) ; BRACE_SQUARE_RIGHT 
    { LVALUE_Index { expr = arr ; index = ind } } 
      // %prec ARR_INDEX
  | e = parse_node(parse) ; DOT ; id = identifier
    { LVALUE_Field { expr = e ; field = id } } 
      // %prec FIELD_SELECT

%inline parse_op:
| AND { BINOP_And }
| OR { BINOP_Or }
| PLUS { BINOP_Add }
| MINUS { BINOP_Sub }
| TIMES { BINOP_Mult }
| DIV { BINOP_Div }
| MOD { BINOP_Rem }

%inline parse_relation:
  | EQ { RELOP_Eq }
  | NEQ { RELOP_Ne }
  | LT { RELOP_Lt }
  | GT { RELOP_Gt }
  | LE { RELOP_Le }
  | GE { RELOP_Ge }

parse_expression:
  | x = parse_expression_legal_in_lvalue
    { x } 
    %prec UPPER
  | y = parse_expression_remaining
    { y } 
    // %prec UPPER

parse_expression_legal_in_lvalue:
  | id = identifier { EXPR_Id id }
  | v = parse_lvalue_aux(parse_expression_legal_in_lvalue)
    { expr_of_lvalue v }
  
  | TRUE { EXPR_Bool true }
  | FALSE { EXPR_Bool false }
  | int = INTEGER { EXPR_Int int }
  | ch = CHAR { EXPR_Char ch }
  | str = STRING { EXPR_String str }
  | LENGTH ; BRACE_LEFT ; e = parse_node(parse_expression) ; BRACE_RIGHT
    { EXPR_Length e }
  | id = identifier ; BRACE_LEFT ; args = separated_list(COMMA, parse_node(parse_expression)) ; BRACE_RIGHT
    { EXPR_Call { callee = id ; arguments = args } } 
      // %prec FUNC_CALL

  | e = parse_unfold_aux(parse_expression_legal_in_lvalue)
    { e } 
      // %prec UNFOLDING

parse_expression_remaining:
  | v = parse_lvalue_aux(parse_expression_remaining)
    { expr_of_lvalue v }

  | BRACE_CURLY_LEFT ; elems = parse_optionaly_ended_nonempty_separeted_list(COMMA, parse_node(parse_expression)) ; BRACE_CURLY_RIGHT
    { EXPR_Array elems }
  | BRACE_CURLY_LEFT ; fields = parse_optionaly_ended_nonempty_separeted_list(COMMA, parse_node(parse_field_expression)) ; BRACE_CURLY_RIGHT
    { EXPR_Record fields }
  | BRACE_CURLY_LEFT ; BRACE_CURLY_RIGHT
    { EXPR_EmptyStruct }
  | BRACE_LEFT ; e = parse_expression ; BRACE_RIGHT
    { e }
  
  | e = parse_relation_aux(parse_expression)
    { e }
  | e = parse_unfold_aux(parse_expression_remaining)
    { e }
      //  %prec UNFOLDING

  | l = parse_node(parse_expression) ; op = parse_op ; r = parse_node(parse_expression)
    { EXPR_Binop {op = op ; lhs = l ; rhs = r }}
  | NOT ; e = parse_node(parse_expression)
    { EXPR_Unop { op = UNOP_Not ; expr = e} } %prec NOT
  | MINUS ; e = parse_node(parse_expression)
    { EXPR_Unop { op = UNOP_Neg ; expr = e} } %prec MINUS_UNOP

parse_relation_aux(parse):
  | l = parse_node(parse) ; rel = parse_relation ; r = parse_node(parse_expression)
    { EXPR_Relation { op = rel ; lhs = l ; rhs = r } }

%inline parse_unfold_aux(parse):
  | e = parse_node(parse) ; UPPER
    { EXPR_Unfold e }

parse_field_expression:
  | id = identifier ; EQUALS ; e =  parse_node(parse_expression)
    { FieldExpr { name = id ; value = e } }

parse_type_expression: 
  | BOOL { TEXPR_Bool }
  | INT { TEXPR_Int }
  | id = identifier { TEXPR_Id id }
  | tp = parse_node(parse_type_expression) ; BRACE_SQUARE_BOTH
    { TEXPR_Array tp }
  | BRACE_CURLY_LEFT ; fields = parse_optionaly_ended_separeted_list(COMMA, parse_node(parse_var_declaration)) ; BRACE_CURLY_RIGHT
    { TEXPR_Record (List.map (fmap_node @@ function VarDecl { id=id ; tp=tp } -> FieldType {name=id; tp=tp}) fields) }

parse_var_declaration: 
  | id = identifier ; COLON ; typ = parse_node(parse_type_expression)
    { VarDecl {id = id; tp = typ} }

// inline?
parse_opt_type_decl:
  | COLON ; x = parse_node(parse_type_expression) { Some x }
  | { None }

// same as above
parse_opt_assignment: 
  | EQUALS ; x = parse_node(parse_expression)
    { Some x }
  | { None }

parse_alternative_same_type(a, b):
  | x = a ; { x }
  | x = b ; { x }

parse_statement_not_empty_return:
  | id = identifier ; BRACE_LEFT ; args = separated_list(COMMA, parse_node(parse_expression)) ; BRACE_RIGHT
    { STMT_Call { callee = id ; arguments = args } }
  | l = parse_node(parse_lvalue) ; EQUALS ; e = parse_node(parse_expression)
    { STMT_Assign { lhs = l ; rhs = e } }
  | vardec = parse_node(parse_var_declaration) ; opt_assignment = parse_opt_assignment;
    { STMT_VarDecl
      { var = vardec ; 
        init = opt_assignment }}
  | id = identifier ; COLON ; tpl = parse_array_type_with_sizes
    { match tpl with 
      | (tp, sizes) -> STMT_ArrayDecl {id = id ; tp = tp ; sizes = sizes } }
  | IF ; bl = parse_node(parse_expression) ; s1 = parse_node(parse_statement) ; s2 = preceded(ELSE, parse_node(parse_statement))
    { STMT_If { cond = bl ; then_branch = s1 ; else_branch = Some s2 } }
      // %prec IF_ELSE
  | IF ; bl = parse_node(parse_expression) ; s1 = parse_node(parse_statement) ;
    { STMT_If { cond = bl ; then_branch = s1 ; else_branch = None } } %prec ELSE
      // %prec IF_ELSE
  | WHILE ; e = parse_node(parse_expression) ; s1 = parse_node(parse_statement)
    { STMT_While { cond = e ; body = s1 } }
  | BRACE_CURLY_LEFT ; xs = list( parse_alternative_same_type(
      terminated(parse_node(parse_statement_not_empty_return), option(SEMICOLON)),
      parse_node(parse_empty_return_stmt))) ; BRACE_CURLY_RIGHT
    { STMT_Block xs }
  | RETURN ; ret = parse_node(parse_expression)
    { STMT_Return (Some ret) }

parse_empty_return_stmt:
  | RETURN ; SEMICOLON
    { STMT_Return None }

parse_statement:
  | x = parse_empty_return_stmt ; { x }
  | x = parse_statement_not_empty_return ; { x }


parse_array_type_with_sizes: 
  | tp0 = parse_node(parse_type_expression) ; sizes = nonempty_list(parse_node(delimited(BRACE_SQUARE_RIGHT, parse_expression, BRACE_SQUARE_RIGHT)))
    // using [] bracket locations for TEXPR_Array node locations
    { 
      (List.fold_left (fun tp size_node -> (fmap_node (Fun.const @@ TEXPR_Array tp) size_node)) tp0 sizes , sizes)
    }

parse_gdef_function: 
  | id = identifier ; BRACE_LEFT ; 
    args = separated_list(COMMA, parse_node(parse_var_declaration)) ;
    BRACE_RIGHT ;
    opt_typ = parse_opt_type_decl;
    body = parse_node(parse_statement)
    { GDEF_Function { id = id; formal_parameters = args; return_type = opt_typ; body = body} } 

parse_gdef_type:
  | TYPE ; id = identifier ; EQUALS ; tp=parse_node(parse_type_expression) 
    { GDEF_Type { id=id ; body=tp} }

parse_gdef_use:
  | USE ; id = identifier 
    { GDEF_Use id }

parse_global_definition: 
  | x = parse_gdef_type
    { x }
  | x = parse_gdef_use
    { x }
  | x = parse_gdef_function
    { x }

parse_global_declaration:
  | x = parse_gdef_type
    { match x with 
      | GDEF_Type { id ; body} -> GDECL_Type { id ; body} 
      | _ -> failwith "Impossible, expected GDEF_Type" }

  | id = identifier ; BRACE_LEFT ; 
    args = separated_list(COMMA, parse_node(parse_var_declaration)) ;
    BRACE_RIGHT ;
    opt_typ = parse_opt_type_decl;
    { GDECL_Function { id = id ; formal_parameters = args ; return_type = opt_typ } }
  | TYPE ; id = identifier; 
    { GDECL_TypeDecl id }

(* Oraz pusty interfejs *)
interface_file:
  | xs = list(parse_node(parse_global_declaration)) ; EOF
  { ModuleInterface {global_declarations=xs} }

(* Obecnie potrafimy sparsować tylko pusty plik (wymagamy od razu tokena EOF) *)

source_file:
  | xs = list(parse_node(parse_global_definition)) ; EOF
  { ModuleDefinition {global_definitions=xs} }

%inline parse_node(parse):
  x = parse
    {mk_node $loc x}

identifier:
    | id = IDENTIFIER
    { Identifier id }

(* 
   ** przykład użycia mk_node

    atomic_expression:
        | identifier
        { mk_node $loc (EXPR_Id $1) }
*)

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)
