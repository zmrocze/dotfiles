type location = Location of { line : int; column : int; file : string }

let string_of_location (Location { line; column; file }) =
  Format.sprintf "%s:%u:%u" file line column

type node_tag = NodeTag of int

let string_of_node_tag (NodeTag i) = Format.sprintf "%%node%i" i

(* Tworzy świeży identyfikator *)
let fresh_node_tag =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    NodeTag r

(* Węzeł składni abstrakcyjnej.
 * Zawiera informację o położeniu oraz unikatowy identyfikator.
 *)
type 'a node = { loc : location; tag : node_tag; data : 'a }
type identifier = Identifier of string

module IdMap = Map.Make (struct
  type t = identifier

  let compare = compare
end)

let string_of_identifier (Identifier x) = x

type binop =
  | BINOP_And
  | BINOP_Or
  | BINOP_Add
  | BINOP_Sub
  | BINOP_Mult
  | BINOP_Div
  | BINOP_Rem

let string_of_binop = function
  | BINOP_And -> "and"
  | BINOP_Or -> "or"
  | BINOP_Add -> "+"
  | BINOP_Sub -> "-"
  | BINOP_Mult -> "*"
  | BINOP_Div -> "/"
  | BINOP_Rem -> "%"

type relop = RELOP_Eq | RELOP_Ne | RELOP_Lt | RELOP_Gt | RELOP_Le | RELOP_Ge

let string_of_relop = function
  | RELOP_Eq -> "=="
  | RELOP_Ne -> "!="
  | RELOP_Lt -> "<"
  | RELOP_Gt -> ">"
  | RELOP_Ge -> ">="
  | RELOP_Le -> "<="

type unop = UNOP_Not | UNOP_Neg

type type_expression = type_expression_data node

and type_expression_data =
  | TEXPR_Id of identifier
  | TEXPR_Int
  | TEXPR_Bool
  | TEXPR_Array of type_expression
  | TEXPR_Record of field_type list

and field_type = field_type_data node
and field_type_data = FieldType of { name : identifier; tp : type_expression }

type expression = expression_data node

and expression_data =
  | EXPR_Id of identifier
  | EXPR_Int of int32
  | EXPR_Char of char
  | EXPR_String of string
  | EXPR_Bool of bool
  | EXPR_Length of expression
  | EXPR_Relation of { op : relop; lhs : expression; rhs : expression }
  | EXPR_Binop of { op : binop; lhs : expression; rhs : expression }
  | EXPR_Unop of { op : unop; expr : expression }
  | EXPR_Call of { callee : identifier; arguments : expression list }
  | EXPR_Index of { expr : expression; index : expression }
  | EXPR_Field of { expr : expression; field : identifier }
  | EXPR_Unfold of expression
  | EXPR_Array of expression list
  | EXPR_Record of field_expression list
  | EXPR_EmptyStruct

and field_expression = field_expression_data node

and field_expression_data =
  | FieldExpr of { name : identifier; value : expression }

let field_subexpr { data = FieldExpr { value; _ }; _ } = value

type var_declaration = var_declaration_data node

and var_declaration_data =
  | VarDecl of { id : identifier; tp : type_expression }

let identifier_of_var_declaration { data = VarDecl { id; _ }; _ } = id
let type_expression_of_var_declaration { data = VarDecl { tp; _ }; _ } = tp

type lvalue = lvalue_data node

and lvalue_data =
  | LVALUE_Id of identifier
  | LVALUE_Index of { expr : expression; index : expression }
  | LVALUE_Field of { expr : expression; field : identifier }

type statement = statement_data node

and statement_data =
  | STMT_Call of { callee : identifier; arguments : expression list }
  | STMT_Assign of { lhs : lvalue; rhs : expression }
  | STMT_VarDecl of { var : var_declaration; init : expression option }
  | STMT_ArrayDecl of {
      id : identifier;
      tp : type_expression;
      sizes : expression list;
    }
  | STMT_If of {
      cond : expression;
      then_branch : statement;
      else_branch : statement option;
    }
  | STMT_While of { cond : expression; body : statement }
  | STMT_Return of expression option
  | STMT_Block of statement list

type global_definition = global_definition_data node

and global_definition_data =
  | GDEF_Use of identifier
  | GDEF_Type of { id : identifier; body : type_expression }
  | GDEF_Function of {
      id : identifier;
      formal_parameters : var_declaration list;
      return_type : type_expression option;
      body : statement;
    }

type global_declaration = global_declaration_data node

and global_declaration_data =
  | GDECL_Type of { id : identifier; body : type_expression }
  | GDECL_TypeDecl of identifier
  | GDECL_Function of {
      id : identifier;
      formal_parameters : var_declaration list;
      return_type : type_expression option;
    }

type module_definition =
  | ModuleDefinition of { global_definitions : global_definition list }

type module_interface =
  | ModuleInterface of { global_declarations : global_declaration list }
