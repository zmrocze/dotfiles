open Zi_lib
open Ast
open Types

let (<<) f g = fun x -> f (g x);;
let void = Fun.const ()

let throwBool fail bl = if bl then () else fail ()

let rec forM f = function 
  | [] -> Some []
  | x :: xs -> Option.bind (f x) (fun y -> Option.bind (forM f xs) (fun ys -> Some (y :: ys)))

let maybe_eq eq a b = match (a,b) with 
  | None, None  -> true
  | Some x, Some y -> eq x y
  | _ , _ -> false

let for_all2 f a b = List.length a == List.length b && List.for_all2 f a b

let errorReporter er = 
  er.logf "Failed ups";
  er.fail () 

let rec eq_normal_type a b = 
  match (a,b) with 
  | (TP_Id id1), TP_Id id2 -> id1 == id2
  | TP_Int, TP_Int -> true
  | TP_Bool, TP_Bool -> true
  | TP_Array tp1, TP_Array tp2 -> eq_normal_type tp1 tp2
  | TP_Record list1, TP_Record list2 -> 
    let eq_pairs (id1, tp1) (id2, tp2) = id1 == id2 && eq_normal_type tp1 tp2 
    in for_all2 eq_pairs list1 list2
  | _, _ -> false

(* Pomocnicza funkcja sprawdzająca czy lista zawiera
 * obiekty o unikalnych nazwach
 *)
let check_uniqueness xs ~name_of ~loc_of ~report_error =
  let loc_map = Hashtbl.create 32 in
  let rec loop xs =
    match xs with
    | [] -> ()
    | x :: xs -> (
      match name_of x with
        | name -> (
          match Hashtbl.find_opt loc_map name with
          | None ->
              Hashtbl.add loc_map name (loc_of x);
              loop xs
          | Some prev_loc -> report_error x prev_loc))
  in
  loop xs

module Make (Toolbox : Iface.TYPECHECKER_TOOLBOX) = struct
  (* Moduł pozwalający na zgłaszanie błędów, a także przerwanie sprawdzania
   * typów (w przypadku gdy błąd nie pozwala na dalszą pracę) za pomocą
   * funkcji `ErrorReporter.fail`
   *)
  module ErrorReporter = Toolbox.ErrorReporter

  (* Logger *)
  let logf fmt = Logger.make_logf __MODULE__ fmt

  (* Moduł odpowiedzialny za sprawdzanie poprawności wyrażeń typowych
   * i tłumaczenie ich do formatu używanego przez typechecker
   *)
  module TypeExprChecker = struct
    let rec check_type_expression top_env (tp : type_expression) : normal_type =
      match tp.data with
      | TEXPR_Id x -> (
          (* Sprawdź czy jest to istniejąca nazwa typu *)
          (match IdMap.find_opt x top_env with
            | Some (ST_AbstractType _) -> ()
            | Some (ST_TypeDef _) -> ()
            | _ -> ErrorReporter.fail () );
          TP_Id x;
        )
      | TEXPR_Int -> TP_Int
      | TEXPR_Bool -> TP_Bool
      | TEXPR_Array tp -> (fun x -> TP_Array x) @@ check_type_expression top_env tp
      | TEXPR_Record flds -> (fun x -> TP_Record x) @@ let 
          (names, tps) = List.split @@ List.map (fun x -> x.data |> function FieldType r -> (r.name, r.tp)) flds
    in let 
      normal_tps = List.map (check_type_expression top_env) tps
  in begin check_uniqueness (List.mapi (fun x y -> (x,y)) names) ~name_of:snd ~loc_of:fst ~report_error:(fun x y -> ErrorReporter.fail ());
    List.combine names normal_tps
    end

    let check_formal_parameters top_env =
      List.map (fun { data = VarDecl { tp; _ }; _ } ->
          check_type_expression top_env tp)

    let check_return_type top_env = function
      | None -> None
      | Some tp -> Some (check_type_expression top_env tp)
  end

  module TopEnv = struct
    type t = global_symbols

    let empty = IdMap.empty

    let declare_type top_env ~loc id =
      (* Dodaj deklarację typu do znanych symboli,
       * pamiętaj o poprawnym obsłużeniu przesłaniania nazw
       *)
      match IdMap.find_opt id top_env with 
        | None 
        | Some (ST_AbstractType _) -> IdMap.add id (ST_AbstractType {loc}) top_env
        | Some (ST_TypeDef _) -> top_env
        | Some (ST_Function _) -> ErrorReporter.fail ()

    let define_type top_env ~loc id body =
      (* Dodaj definicję typu do znanych symboli, pamiętaj aby sprawdzić
       * potencjalne konflikty z istniejącą już definicją lub deklaracją
       *)
       match IdMap.find_opt id top_env with 
       | None 
       | Some (ST_AbstractType _) -> IdMap.add id (ST_TypeDef {loc; body}) top_env
       | Some (ST_TypeDef {loc; body=body1}) -> if eq_normal_type body body1 then 
          top_env else ErrorReporter.fail ()
       | Some (ST_Function _) -> ErrorReporter.fail ()

    let declare_function top_env ~loc id param_types return_type =
      (* Dodaj deklarację funkcji do znanych symboli, pamiętaj aby sprawdzić
       * potencjalne konflikty z istniejącą już deklaracją
       *)
       match IdMap.find_opt id top_env with 
       | None -> IdMap.add id (ST_Function {loc; param_types; return_type}) top_env
       | Some (ST_Function {loc=loc1; param_types=pr_tps1;return_type=ret_tp1}) -> 
        if (for_all2 eq_normal_type param_types pr_tps1) && maybe_eq eq_normal_type ret_tp1 return_type then 
           IdMap.add id (ST_Function {loc; param_types; return_type}) top_env
        else ErrorReporter.fail ()
       | Some (ST_AbstractType _) -> ErrorReporter.fail ()
       | Some (ST_TypeDef {loc; body=body1}) -> ErrorReporter.fail ()
       

    let use_interface top_env intf =
      let use_interface_entity id sym top_env =
        match sym with
        | ST_AbstractType { loc } -> declare_type ~loc top_env id
        | ST_TypeDef { loc; body } -> define_type ~loc top_env id body
        | ST_Function { loc; param_types; return_type } ->
            declare_function ~loc top_env id param_types return_type
      in
      IdMap.fold use_interface_entity intf top_env
  end

  module InterfaceChecker = struct
    let collect_type_names top_env (ModuleInterface { global_declarations }) =
      let scan top_env decl =
        match decl.data with
        | GDECL_Type { id; _ } | GDECL_TypeDecl id ->
            TopEnv.declare_type ~loc:decl.loc top_env id
        | GDECL_Function _ -> top_env
      in
      List.fold_left scan top_env global_declarations

    let collect_function_types_and_type_definitions top_env
        (ModuleInterface { global_declarations }) =
      let scan top_env decl =
        match decl.data with
        | GDECL_Type { id; body } ->
            TopEnv.define_type top_env ~loc:decl.loc id (TypeExprChecker.check_type_expression top_env body)
        | GDECL_TypeDecl _ -> top_env (*  dlaczego nie dodamy ST_AbstractType? <- jest już po collect_names *)
        | GDECL_Function { id; formal_parameters; return_type } ->
            (* Sprawdź poprawność i dodaj deklarację funkcji do top_env *)
            TopEnv.declare_function top_env ~loc:decl.loc id
              (TypeExprChecker.check_formal_parameters top_env formal_parameters)
              (TypeExprChecker.check_return_type top_env return_type)
      in
      List.fold_left scan top_env global_declarations

    let check_interface idef =
      let top_env = TopEnv.empty in
      let top_env = collect_type_names top_env idef in
      let top_env = collect_function_types_and_type_definitions top_env idef in
      top_env
  end

  (* Hashtablica którą zwracamy jak wszystko jest OK.
   * Mapuje znacznik węzła na przypisany typ *)
  let node2type_map = Hashtbl.create 513

  (* Tablica pamiętająca typy funkcji. (dla każdego tagu definicji)
   * Zapamiętujemy typy funkcji, by nie tłumaczyć ich na wewnętrzną
   * reprezentację dwa razy. Ma do duże znaczenie dla raportowania błędów --
   * jeśli jakieś wyrażenie typowe jest niepoprawne, to nie chcemy dwa razy
   * oglądać błędu *)
  let function_types = Hashtbl.create 42

  (* Środowisko *)
  module Env = struct
    type t = {
      return_type : normal_type option;
      top_env : TopEnv.t;
      local_variables : normal_type IdMap.t;
    }

    let create top_env return_type =
      { return_type; top_env; local_variables = IdMap.empty }

    let extend ~loc env id tp =
      match IdMap.find_opt id env.local_variables with
      | None ->
          { env with local_variables = IdMap.add id tp env.local_variables }
      | _ ->
          (* Zgłaszamy błąd ale nie przerywamy działania *)
          ErrorReporter.report_shadows_previous_definition ~loc ~id;
          env

    let lookup_local env id = IdMap.find_opt id env.local_variables
    let lookup_global env id = IdMap.find_opt id env.top_env
    (* let lookup env id = let x = lookup_local env id  
      in if Option.is_none x then 
        lookup_global env id
      else x *)
    let top_env env = env.top_env
    let return_type env = env.return_type
  end

  (* 
   * Procedury inferencji i sprawdzania. Dla implementacyjnej wygody dwie
   * główna funkcja funkcja inferencji typu jest wraperem co wpisuje
   * wynik udanej inferencji do hashtablicy node2type_map.
   *)
  let rec infer_expression env e =
    match _infer_expression env e with
    | Some tp ->
        Hashtbl.replace node2type_map e.tag tp;
        logf "%s: inferred type %s" (string_of_location e.loc)
          (string_of_normal_type tp);
        Some tp
    | None ->
        logf "%s: failed" (string_of_location e.loc);
        None

  (* Rzeczywista funkcja inferencji. Implementacja mniej więcej jak w
   * specyfikacji. Chcemy aby
   *
   * infer_expression G e = Some T
   *    oznaczało, że zachodzi
   * G |- e : T 
   *)
  and _infer_expression env e =
    match e.data with
    | EXPR_EmptyStruct ->
        ErrorReporter.report_cannot_infer ~loc:e.loc;
        None
    | EXPR_Id id -> Env.lookup_local env id
    | EXPR_Int _ | EXPR_Char _ -> Some TP_Int
    (* Literały łańcuchowe to tablice intów *)
    | EXPR_String _ -> Some (TP_Array TP_Int)
    | EXPR_Bool _ -> Some TP_Bool
    (* Wyinferuj argument i zwróć info że wyrażenie ma typ int *)
    | EXPR_Length arg -> begin match infer_expression env arg with
      | Some (TP_Array _) -> Some TP_Int
      | _ -> None
      end
    (* Relacje porządku się prosto sprawdza, wymagamy po prostu aby
     * podwyrażenia były typu int, bo tylko na nim mamy porządek. *)
    | EXPR_Relation { lhs; rhs; op = RELOP_Ge }
    | EXPR_Relation { lhs; rhs; op = RELOP_Gt }
    | EXPR_Relation { lhs; rhs; op = RELOP_Lt }
    | EXPR_Relation { lhs; rhs; op = RELOP_Le } ->
      if check_expression env lhs TP_Int && check_expression env rhs TP_Int then 
        Some TP_Bool
    else None
    (* Równość jest bardziej subtelna. Na każdym typie mamy równosć.
     * Inferujemy lewą stronę i sprawdzamy prawą kontra wynik.
     * Gdy się nie uda to dla wygody użytkownika inferujemy typ prawej strony
     * tylko po to aby być-może zgłosić jakieś błędy tam.
     *)
    | EXPR_Relation { lhs; rhs; op = RELOP_Eq }
    | EXPR_Relation { lhs; rhs; op = RELOP_Ne } ->
      Option.bind (infer_expression env lhs) (fun l ->
        if check_expression env rhs l then 
          Some TP_Bool 
    else let _ = infer_expression env rhs in None)
    (* Dodawnaie jest podobne *)
    | EXPR_Binop { lhs; rhs; op = BINOP_Add } -> 
      Option.bind (infer_expression env lhs) (fun l ->
          if check_expression env rhs l && ((match l with | TP_Array _ -> true | _ -> false ) || l == TP_Int )  then 
            Some l
      else let _ = infer_expression env rhs in None)
    (* Operatory logiczne śmigają tylko z boolami *)
    | EXPR_Binop { lhs; rhs; op = BINOP_And; _ }
    | EXPR_Binop { lhs; rhs; op = BINOP_Or; _ } -> 
      if check_expression env lhs TP_Bool && check_expression env rhs TP_Bool then 
        Some TP_Bool else None
        
    (* ...a arytmetyczne tylko z intami *)
    | EXPR_Binop { lhs; rhs; op = BINOP_Sub; _ }
    | EXPR_Binop { lhs; rhs; op = BINOP_Rem; _ }
    | EXPR_Binop { lhs; rhs; op = BINOP_Mult; _ }
    | EXPR_Binop { lhs; rhs; op = BINOP_Div; _ } ->
      if check_expression env lhs TP_Int && check_expression env rhs TP_Int then 
        Some TP_Int else None
    (* Negacja tylko z intem *)
    | EXPR_Unop { op = UNOP_Neg; expr; _ } -> if (check_expression env expr TP_Int) then Some TP_Int else None  
    (* Negacja logiczna tylko z boolem *)
    | EXPR_Unop { op = UNOP_Not; expr; _ } -> if (check_expression env expr TP_Bool) then Some TP_Bool else None  
    (* Sprawdzenie wywołania *)
    | EXPR_Call { callee; arguments } -> begin match (Env.lookup_global env callee) with 
      | Some (ST_Function {loc; param_types; return_type = Some return_type}) -> if for_all2 (check_expression env) arguments param_types then
        Some return_type else None
      | _ -> None
      end
    (* inferencja expr[index] oznacza inferencje `expr` i sprawdzenie że
     * wynik jest postaci A[] gdzie A to jakiś typ. Index sprawdzamy
     * kontra typ `int` *)
    | EXPR_Index { expr; index } -> begin match infer_expression env expr with 
      | Some (TP_Array a) -> if check_expression env index TP_Int then 
          Some a else None
      | _ -> None
    end
    | EXPR_Field { expr; field } -> begin match infer_expression env expr with 
      | Some (TP_Record l) -> List.find_map (function (ide, t) -> if ide == field then Some t else None) l
      | _ -> None
    end
    | EXPR_Unfold expr -> (match infer_expression env expr with 
      | Some (TP_Id id) -> (match Env.lookup_global env id with 
        | Some (ST_TypeDef {loc=_; body}) -> Some body
        | _ -> None
          )
      | _ -> None
      )
    (* Pusta tablica lub rekord, nie powinno się nigdy zdarzyć *)
    | EXPR_Array [] | EXPR_Record [] -> assert false
    (* Inferujemy typ pierwszego elementu i sprawdzamy resztę
     * kontra uzyskany wynik. Infer na {...} będzie tylko działał w przypadku
     * `length` i jest mało istotny.
     *)
    | EXPR_Array (x :: xs) -> Option.bind (infer_expression env x) (fun tx -> 
      if List.for_all (fun y -> check_expression env y tx) xs then 
        Some (TP_Array tx) else None)
    | EXPR_Record flds -> let  
      mflds = List.map (fun x -> x.data |> function FieldExpr {name=nm; value=ex} -> Option.map (fun x -> (nm, x)) (infer_expression env ex)) flds in let
      tpflds = List.fold_left (fun ml mfld -> Option.bind ml (fun l -> Option.bind mfld (fun fld -> Some (fld :: l)))) (Some []) mflds
    in Option.map (fun x -> TP_Record x) tpflds
      (* for flds ( \FieldExpr {name, exr} -> (name, ) <$> infer_expression env exr) *)

  (* Sprawdź wyrażenie kontra oczekiwany typ. Ta procedura jest używana
   * w większości przypadku bo niemalże wszędzie mamy jakieś oczekiwanie.
   *
   * Jej rola to obsłużenie specjalnych przypadków gdzie potrafimy z
   * oczekiwania coś wyłuskać i pchać w dół nowe oczekiwanie. 
   * Gdy nie mamy specjalnego przypadku to jest to zwykły fallback do
   * procedury inferencji.
   *)
  and check_expression env e expected =
    Hashtbl.replace node2type_map e.tag expected;
    (match (e.data, expected) with
    | EXPR_EmptyStruct, (TP_Array _ | TP_Record []) -> ()
    | EXPR_EmptyStruct, _ ->
        ErrorReporter.report_type_mismatch ~loc:e.loc ~actual:(TP_Record [])
          ~expected
    (* Jeżeli oczekujemy dodawania tablic to sprawdzamy podwyrażenia
     * pchając w dół info że mają być oczekiwanego typu. Gdy chcemy
     * dodać inty to fallback do inferencji sobie poradzi. *)
    | EXPR_Binop { op = BINOP_Add; lhs; rhs }, (TP_Array _ | TP_Int) -> 
        let _ = check_expression env lhs expected in
        let _ = check_expression env rhs expected in ();
    | EXPR_Binop { op = BINOP_Add; lhs; rhs }, _ -> ErrorReporter.fail ()
    | EXPR_Index { index; expr }, _ -> 
        let _ = check_expression env index TP_Int in 
        let _ = check_expression env expr (TP_Array expected) in ()
    | EXPR_Array elements, TP_Array tp -> 
        let _ = List.for_all (fun x -> check_expression env x tp) elements in ()
    | EXPR_Record fields, TP_Record ftypes -> 
      let _ = for_all2 (fun x (nm, tp) -> 
        match x.data with FieldExpr {name; value} -> 
          name == nm && check_expression env value tp )
        fields ftypes
      in ()
    (* Fallback *)
    | ( ( EXPR_Id _ | EXPR_Int _ | EXPR_Char _ | EXPR_String _ | EXPR_Bool _
        | EXPR_Length _ | EXPR_Relation _ | EXPR_Binop _ | EXPR_Unop _
        | EXPR_Call _ | EXPR_Field _ | EXPR_Unfold _ | EXPR_Array _
        | EXPR_Record _ ),
        _ ) -> (
        match infer_expression env e with
        | Some actual when actual = expected -> ()
        | None -> ()
        | Some actual ->
            ErrorReporter.report_type_mismatch ~loc:e.loc ~actual ~expected));
    true;;

  let check_lvalue env lv = match 
    match lv.data with
    | LVALUE_Id id -> Env.lookup_local env id
    | LVALUE_Index { expr; index } -> let 
      _ = throwBool ErrorReporter.fail @@ check_expression env index TP_Int in
      ( match infer_expression env expr with 
      | None -> None
      | Some (TP_Array a) -> Some a
      | _ -> None)
    | LVALUE_Field { expr; field } -> (match infer_expression env expr with 
      | None -> None 
      | Some (TP_Record ls) -> Option.map (function (_, tp) -> tp) @@ List.find_opt (function (id, _) -> field == id) ls
      | Some _ -> None)
      with 
      | None -> ErrorReporter.fail ()
      | Some x -> x

  (* Sprawdź statementy, dostajemy środowisko i zwracamy środowisko
   * z nowo zdefiniowanymi zmiennymi
   *)
  let rec check_statement env stmt =
    match stmt.data with
    (* Wyinferuj typ `lhs` i sprawdź `rhs` kontra wynik *)
    | STMT_Assign { lhs; rhs} -> 
      let tp = check_lvalue env lhs in 
      let _ = throwBool ErrorReporter.fail @@ check_expression env rhs tp in
      env
    | STMT_Call { callee; arguments } -> begin match Env.lookup_global env callee with
      | None -> ErrorReporter.fail ()
      | Some (ST_Function {return_type=None; param_types=param_types; loc}) -> 
        begin match forM (infer_expression env) arguments with 
          | None -> ErrorReporter.fail ()
          | Some ts -> if for_all2 eq_normal_type ts param_types then 
            env else ErrorReporter.fail ()
      end
      | Some _ -> ErrorReporter.fail ()
    end
    | STMT_VarDecl { var = { data = VarDecl { id; tp }; loc; _ }; init } ->
        let tp = TypeExprChecker.check_type_expression (Env.top_env env) tp in
        let env = Env.extend ~loc env id tp in
        (match init with None -> () | Some e -> void @@ check_expression env e tp);
        env
    | STMT_ArrayDecl { id; tp=tp_n; sizes } -> 
      let tp = TypeExprChecker.check_type_expression (Env.top_env env) tp_n in 
      (* TODO, tp counts sizes arrays? *)
      let env = Env.extend ~loc:tp_n.loc env id tp in
      (List.iter (fun e -> throwBool ErrorReporter.fail @@ check_expression env e TP_Int) sizes);
        env
    | STMT_If { cond; then_branch; else_branch } -> begin
        throwBool ErrorReporter.fail @@ check_expression env cond TP_Bool;
        let _ = check_statement env then_branch in
        let _ = Option.map (check_statement env) else_branch in
        env
      end
    | STMT_While { cond; body } ->
        void @@ check_expression env cond TP_Bool;
        let (_ : Env.t) = check_statement env body in
        env
    | STMT_Return rv -> begin match rv with
      | None -> (match Env.return_type env with | None -> env | Some _ -> ErrorReporter.fail ())
      | Some v -> (match Env.return_type env with 
        | None -> ErrorReporter.fail ()
        | Some t -> if check_expression env v t then env else ErrorReporter.fail ()
        )
    end 
    | STMT_Block block ->
        let (_ : Env.t) = List.fold_left check_statement env block in
        env

  (* syntaktyczne sprawdzenie, czy komenda zawsze wraca *)
  let rec always_returns stmt =
    match stmt.data with
    | STMT_Assign _ | STMT_Call _ | STMT_VarDecl _ | STMT_ArrayDecl _
    | STMT_While _
    | STMT_If { else_branch = None; _ } ->
        false
    | STMT_Return _ -> true
    | STMT_If { then_branch; else_branch = Some else_branch; _ } ->
        always_returns then_branch && always_returns else_branch
    | STMT_Block stmts -> List.exists always_returns stmts

  let check_return_paths ~loc ~id body return_type =
    match return_type with
    | None -> () (* Jest domyślny powrót na końcu funkcji *)
    | Some _ ->
        if not (always_returns body) then
          ErrorReporter.report_not_all_control_paths_return_value ~loc ~id

  let check_formal_parameters =
    List.fold_left2 (fun env { data = VarDecl { id; _ }; loc; _ } tp ->
        Env.extend ~loc env id tp)

  let collect_type_names_and_interfaces top_env
      (ModuleDefinition { global_definitions }) =
    let scan top_env def =
      match def.data with
      | GDEF_Function _ ->
          (* To będziemy robić w późniejszych etapach *)
          top_env
      | GDEF_Use (Identifier name) ->
          let iface = Toolbox.find_interface ~loc:def.loc name in
          let iface_env = InterfaceChecker.check_interface iface in
          TopEnv.use_interface top_env iface_env
      | GDEF_Type { id; _ } -> TopEnv.declare_type top_env ~loc:def.loc id
    in
    List.fold_left scan top_env global_definitions

  (* Znajdź i sprawdź wszystkie definicje typów oraz typy funkcji *)
  let collect_function_types_and_type_definitions top_env
      (ModuleDefinition { global_definitions }) =
    let scan top_env def =
      match def.data with
      | GDEF_Use _ ->
          (* Ten przypadek obsłużyliśmy w poprzednim etapie *)
          top_env
      | GDEF_Type { id; body } ->
          let tp = TypeExprChecker.check_type_expression top_env body in
          TopEnv.define_type top_env ~loc:def.loc id tp
      | GDEF_Function { id; formal_parameters; return_type; _ } ->
          let ftp =
            TypeExprChecker.check_formal_parameters top_env formal_parameters
          in
          let rtp = TypeExprChecker.check_return_type top_env return_type in
          Hashtbl.add function_types def.tag (ftp, rtp);
          TopEnv.declare_function top_env ~loc:def.loc id ftp rtp
    in
    List.fold_left scan top_env global_definitions

  (* Sprawdź ciało funkcji *)
  let check_global_definition top_env def =
    match def.data with
    | GDEF_Use _ | GDEF_Type _ ->
        (* Te przypadki były już obsłużone w poprzednich etapach *)
        ()
    | GDEF_Function { id; formal_parameters; body; _ } ->
        let ftp, rtp = Hashtbl.find function_types def.tag in
        let env = Env.create top_env rtp in
        let env = check_formal_parameters env formal_parameters ftp in
        let (_ : Env.t) = check_statement env body in
        check_return_paths ~loc:def.loc ~id body rtp

  let check_global_definitions top_env (ModuleDefinition { global_definitions })
      =
    List.iter (check_global_definition top_env) global_definitions

  let check_function_uniqueness (ModuleDefinition { global_definitions }) =
    let name_of def =
      match def.data with GDEF_Function { id; _ } -> Some id | _ -> None
    in
    let report_error def prev_loc =
      match def.data with
      | GDEF_Function { id; _ } ->
          ErrorReporter.report_other_error ~loc:def.loc
            ~descr:"This function is defined more than once"
      | _ -> assert false
    in
    check_uniqueness global_definitions ~name_of
      ~loc_of:(fun def -> def.loc)
      ~report_error

  (* Kolejność deklaracji nie ma znaczenia, a zarówno funkcje jak i typy
   * mogą być wzajemnie rekurencyjnie. Więc sprawdzamy w trzech etapach.
   * 1. zbieramy nazwy typów oraz definicje z interfejów
   * 2. zbieramy definicje typów i sygnatury funkcji
   * 3. sprawdzamy typy wewnątrz funkcji
   *)
  let check_module mdef =
    check_function_uniqueness mdef;
    let top_env = TopEnv.empty in
    let top_env = collect_type_names_and_interfaces top_env mdef in
    let top_env = collect_function_types_and_type_definitions top_env mdef in
    check_global_definitions top_env mdef;
    (node2type_map, top_env)
end
