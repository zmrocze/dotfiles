(** Reprezentacja typu używana przez sprawdzaczkę typów *)
type normal_type =
  | TP_Id of Ast.identifier
  | TP_Int
  | TP_Bool
  | TP_Array of normal_type
  | TP_Record of (Ast.identifier * normal_type) list

(** Pretty-printer dla typów *)
let rec string_of_normal_type = function
  | TP_Id x -> Ast.string_of_identifier x
  | TP_Int -> "int"
  | TP_Bool -> "bool"
  | TP_Array el -> string_of_normal_type el ^ "[]"
  | TP_Record flds ->
      String.concat ""
        [
          "{";
          String.concat ","
            (flds
            |> List.map (fun (name, tp) ->
                   String.concat ""
                     [
                       Ast.string_of_identifier name;
                       ":";
                       string_of_normal_type tp;
                     ]));
          "}";
        ]

(** Typ globalnego symbolu dostarczanego przez interfejs *)
type symbol_type =
  | ST_AbstractType of { loc : Ast.location }
      (** Typ abstrakcyjny, zdefiniowany przy pomocy deklaracji typu *)
  | ST_TypeDef of { loc : Ast.location; body : normal_type }
      (** Definicja typu *)
  | ST_Function of {
      loc : Ast.location;
      param_types : normal_type list;
      return_type : normal_type option;
    }  (** Funkcja *)

type global_symbols = symbol_type Ast.IdMap.t
(** Reprezentacja interfejsu na której operuje sprawdzaczka typów.
 *  Każdemy symbolowi przyporzątkowujemy informację o tym, czym on jest
 *)
