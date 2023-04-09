open Ast
open Types

type type_checking_error =
  (* Ogólny błąd, że oczekiwany typ się nie zgadza z rzeczywistym *)
  | TCERR_TypeMismatch of {
      loc : location;
      expected : normal_type;
      actual : normal_type;
    }
  (* Wywołaliśmy procedurę/funkcję ze złą liczbą argumentów *)
  | TCERR_BadNumberOfActualArguments of {
      loc : location;
      expected : int;
      actual : int;
    }
  (* Nieznana zmienna lokalna *)
  | TCERR_UnknownLocalVariable of { loc : location; id : identifier }
  (* Nieznany symbol globalny *)
  | TCERR_UnknownGlobalSymbol of { loc : location; id : identifier }
  (* Generyczny błąd gdyby inne nie pasowały *)
  | TCERR_OtherError of { loc : location; descr : string }
  (* Wywołujemy identyfikator, który nie jest funkcją/procedurą *)
  | TCERR_IdentifierIsNotCallable of { loc : location; id : identifier }
  (* Brakuje return gdzieś *)
  | TCERR_NotAllControlPathsReturnValue of { loc : location; id : identifier }
  (* Użyliśmy w wyrażeniu procedury *)
  | TCERR_ExpectedFunctionInsteadOfProcedure of {
      loc : location;
      id : identifier;
    }
  (* Return w procedurze *)
  | TCERR_ProcedureCannotReturnValue of { loc : location }
  (* Brakuje parametru w return *)
  | TCERR_FunctionMustReturnValue of { loc : location }
  (* Typ miał być tablicą *)
  | TCERR_ExpectedArray of { loc : location; actual : normal_type }
  (* Typ miał być rekordem *)
  | TCERR_ExpectedRecord of { loc : location; actual : normal_type }
  (* Podany rekord nie ma pola *)
  | TCERR_NoField of { loc : location; tp : normal_type; field : identifier }
  (* Przykryliśmy nazwę *)
  | TCERR_ShadowsPreviousDefinition of { loc : location; id : identifier }
  (* Generyczny błąd, że nie dało się zrekonstruować typu *)
  | TCERR_CannotInferType of { loc : location }
  (* Pole rekordu jest zdefiniowane więcej niż raz *)
  | TCERR_MultipleFields of {
      loc : location;
      name : identifier;
      prev_loc : location;
    }
  (* Poprzednia definicja symbolu była istotnie inna (np. funckcja zamiast typu) *)
  | TCERR_InconsistentSymbolRedefinition of {
      loc : location;
      id : identifier;
      prev_loc : location;
    }

let string_of_type_checking_error = function
  | TCERR_TypeMismatch { loc; actual; expected } ->
      Format.sprintf "%s: error: type mismatch: expected %s; got %s"
        (string_of_location loc)
        (string_of_normal_type expected)
        (string_of_normal_type actual)
  | TCERR_BadNumberOfActualArguments { loc; actual; expected } ->
      Format.sprintf
        "%s: error: bad number of actual arguments: expected %u; got %u"
        (string_of_location loc) expected actual
  | TCERR_UnknownLocalVariable { loc; id } ->
      Format.sprintf "%s: unknown local variable: %s" (string_of_location loc)
        (string_of_identifier id)
  | TCERR_UnknownGlobalSymbol { loc; id } ->
      Format.sprintf "%s: unknown global symbol: %s" (string_of_location loc)
        (string_of_identifier id)
  | TCERR_IdentifierIsNotCallable { loc; id } ->
      Format.sprintf "%s: identifier is not callable: %s"
        (string_of_location loc) (string_of_identifier id)
  | TCERR_OtherError { loc; descr } ->
      Format.sprintf "%s: error: %s" (string_of_location loc) descr
  | TCERR_NotAllControlPathsReturnValue { loc; id } ->
      Format.sprintf "%s: not all control paths return value: %s"
        (string_of_location loc) (string_of_identifier id)
  | TCERR_ExpectedFunctionInsteadOfProcedure { loc; id } ->
      Format.sprintf "%s: expected function, but a procedure %s was used"
        (string_of_location loc) (string_of_identifier id)
  | TCERR_ExpectedArray { loc; actual } ->
      Format.sprintf "%s: expected array, not: %s" (string_of_location loc)
        (string_of_normal_type actual)
  | TCERR_ExpectedRecord { loc; actual } ->
      Format.sprintf "%s: expected record, not: %s" (string_of_location loc)
        (string_of_normal_type actual)
  | TCERR_NoField { loc; tp; field } ->
      Format.sprintf "%s: record %s has no field named %s"
        (string_of_location loc) (string_of_normal_type tp)
        (string_of_identifier field)
  | TCERR_FunctionMustReturnValue { loc } ->
      Format.sprintf "%s: function must return something"
        (string_of_location loc)
  | TCERR_ProcedureCannotReturnValue { loc } ->
      Format.sprintf "%s: procedure cannot return value"
        (string_of_location loc)
  | TCERR_ShadowsPreviousDefinition { loc; id } ->
      Format.sprintf "%s: shadows previous definition: %s"
        (string_of_location loc) (string_of_identifier id)
  | TCERR_CannotInferType { loc } ->
      Format.sprintf "%s: cannot infer type" (string_of_location loc)
  | TCERR_MultipleFields { loc; name; prev_loc } ->
      Format.sprintf "%s: field %s is defined more than once (e.g. here: %s)"
        (string_of_location loc)
        (string_of_identifier name)
        (string_of_location prev_loc)
  | TCERR_InconsistentSymbolRedefinition { loc; id; prev_loc } ->
      Format.sprintf
        "%s: symbol %s was previously defined as different entity (here: %s)"
        (string_of_location loc) (string_of_identifier id)
        (string_of_location prev_loc)

module type ERROR_REPORTER = sig
  val fail : unit -> 'a
  val wrap : ('a -> 'b) -> 'a -> ('b, type_checking_error list) result

  val report_type_mismatch :
    loc:location -> expected:normal_type -> actual:normal_type -> unit

  val report_other_error : loc:location -> descr:string -> unit
  val report_unknown_local_variable : loc:location -> id:identifier -> unit
  val report_unknown_global_symbol : loc:location -> id:identifier -> unit
  val report_identifier_is_not_callable : loc:location -> id:identifier -> unit

  val report_bad_number_of_arguments :
    loc:location -> expected:int -> actual:int -> unit

  val report_expected_function_instead_of_procedure :
    loc:location -> id:identifier -> unit

  val report_function_must_return_something : loc:location -> unit
  val report_procedure_cannot_return_value : loc:location -> unit
  val report_expected_array : loc:location -> actual:normal_type -> unit
  val report_expected_record : loc:location -> actual:normal_type -> unit

  val report_no_field :
    loc:location -> tp:normal_type -> field:identifier -> unit

  val report_not_all_control_paths_return_value :
    loc:location -> id:identifier -> unit

  val report_shadows_previous_definition : loc:location -> id:identifier -> unit
  val report_cannot_infer : loc:location -> unit

  val report_multiple_fields :
    loc:location -> name:identifier -> prev_loc:location -> unit

  val report_inconsistent_symbol_redefinition :
    loc:location -> id:identifier -> prev_loc:location -> unit
end

module MakeErrorReporter () : ERROR_REPORTER = struct
  exception TypecheckerFailed

  let r = ref []
  let add e = r := e :: !r
  let fail () = raise TypecheckerFailed

  let wrap f x =
    try
      let result = f x in
      match !r with [] -> Ok result | errors -> Error errors
    with TypecheckerFailed -> (
      match !r with
      | [] -> failwith "Typechecker failed but no error was reported"
      | errors -> Error errors)

  let report_type_mismatch ~loc ~expected ~actual =
    add @@ TCERR_TypeMismatch { loc; expected; actual }

  let report_other_error ~loc ~descr = add @@ TCERR_OtherError { loc; descr }

  let report_unknown_local_variable ~loc ~id =
    add @@ TCERR_UnknownLocalVariable { loc; id }

  let report_unknown_global_symbol ~loc ~id =
    add @@ TCERR_UnknownGlobalSymbol { loc; id }

  let report_identifier_is_not_callable ~loc ~id =
    add @@ TCERR_IdentifierIsNotCallable { loc; id }

  let report_bad_number_of_arguments ~loc ~expected ~actual =
    add @@ TCERR_BadNumberOfActualArguments { loc; expected; actual }

  let report_expected_function_instead_of_procedure ~loc ~id =
    add @@ TCERR_ExpectedFunctionInsteadOfProcedure { loc; id }

  let report_function_must_return_something ~loc =
    add @@ TCERR_FunctionMustReturnValue { loc }

  let report_procedure_cannot_return_value ~loc =
    add @@ TCERR_ProcedureCannotReturnValue { loc }

  let report_expected_array ~loc ~actual =
    add @@ TCERR_ExpectedArray { loc; actual }

  let report_expected_record ~loc ~actual =
    add @@ TCERR_ExpectedRecord { loc; actual }

  let report_no_field ~loc ~tp ~field = add @@ TCERR_NoField { loc; tp; field }

  let report_not_all_control_paths_return_value ~loc ~id =
    add @@ TCERR_NotAllControlPathsReturnValue { loc; id }

  let report_shadows_previous_definition ~loc ~id =
    add @@ TCERR_ShadowsPreviousDefinition { loc; id }

  let report_cannot_infer ~loc = add @@ TCERR_CannotInferType { loc }

  let report_multiple_fields ~loc ~name ~prev_loc =
    add @@ TCERR_MultipleFields { loc; name; prev_loc }

  let report_inconsistent_symbol_redefinition ~loc ~id ~prev_loc =
    add @@ TCERR_InconsistentSymbolRedefinition { loc; id; prev_loc }
end
