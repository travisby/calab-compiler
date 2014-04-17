open Utils;;

type st_entree = { typeof: Cst.cst; is_assigned : bool; is_used : bool; pos:Utils.pos}

exception Already_Exists_In_table of st_entree * st_entree;;
exception CannotExitGlobalScope;;
exception IncorrectCSTElementsInSymbolTableError;;
exception Does_Not_Exist_In_Table of st_entree;;

let log_error = Log.log_error_func "symbol_table"
let log_warn = Log.log_warn_func "symbol_table"
let log_trace = Log.log_trace_func "symbol_table"

class ['a, 'b] table =
    object (self)
        val t = Hashtbl.create 0
        method mem x = Hashtbl.mem t x
        method get x = Hashtbl.find t x
        method add (x:'a) (y:'b) =
            if
                self#mem x
            then
                begin
                    log_error ("Symbol is already declared");
                    raise (Already_Exists_In_table ((self#get x), y))
                end
            else
                begin
                    log_trace ("Adding identifier " ^ (Char.escaped x) ^ " to symbol table from line " ^ (string_of_int y.pos.lineno));
                    Hashtbl.add t x y
                end
        method set (x: 'a) (y: 'b) =
            if
                not (self#mem x)
            then
                begin
                    log_error ("Symbol not declared");
                    raise (Does_Not_Exist_In_Table y)
                end
            else
                Hashtbl.replace t x y
        method all () = Hashtbl.fold (fun key value acc -> (key, value) :: acc) t []
    end
;;

(*
 * We use an array because it is a mutable data structure.
 * We will be appending to the array to "add" new scopes
 *)
type scope =
    | Global of (char, st_entree) table * scope array
    (* The last parameter is the parent scope *)
    | Scope of (char, st_entree) table * scope array * scope ref
;;

class symboltable =
    (* A function that takes the unit () as its only parameter *)
    let scope_constructor parent = Scope (new table, [||], parent) in
    let scope_init = Global (new table, [||]) in
    object (self)
        val mutable head = ref scope_init
        val mutable current_scope = ref scope_init
        method private get_current_table = match !current_scope with
                | Scope (t, _, _) -> ref t
                | Global (t, _) -> ref t
        method enter =
            (*
             * We seem (because we actually aren't) to be abusing let..in
             * statements, but it enforces execution order for us, which is not
             * guaranteed by the Ocaml language.  For all of the stuff depending
             * on state (say... moving the scope pointer!) it's important to
             * order the execution of statements
             *)
            let new_scope = ref (scope_constructor current_scope) in
            let scope_array = match !current_scope with
                | Scope (_, xs, _) -> xs
                | Global (_, xs) -> xs
            in
            (* Add the new scope! *)
            let _ = Array.append scope_array [|!new_scope|] in
            let _ = current_scope <- new_scope in
            ()
        method exit =
            let parent_scope = match !current_scope with
                | Scope (_, _, x) -> x
                | Global (_, _) -> raise CannotExitGlobalScope
            in
            let _ = current_scope <- parent_scope in
            ()

        method add id _type = match id with
            | Cst.Id (x, pos) -> !(self#get_current_table)#add x {typeof=_type; is_assigned=false; is_used=false; pos=pos}
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method private get_symbol_table scope = match scope with
            | Scope (t, _, _) -> t
            | Global (t, _) -> t
        method private get_containing_st_pointer id = match id with
            | Cst.Id (x, pos) ->
                    let get_parent scope = match scope with
                        | Scope (_, _, x) -> x
                        | Global (_, _) -> raise CannotExitGlobalScope
                    in
                    (* reference to the dereference just allows us to have a mutable property *)
                    let temp_scope_pointer = ref !current_scope in
                    (*
                     * If temp_score_pointer = head, then we are at the global
                     * scope.  Let it error here if it wants.  Also stop if we
                     * the value exists here!
                     *)
                    while (temp_scope_pointer <> head && not ((self#get_symbol_table !temp_scope_pointer)#mem x)) do
                        (* ocaml will only take a dereference... *)
                        temp_scope_pointer := !(get_parent !temp_scope_pointer)
                    done;
                    if
                        not ((self#get_symbol_table !temp_scope_pointer)#mem x)
                    then
                        begin
                            log_error ("Symbol not declared");
                            raise (Does_Not_Exist_In_Table {pos=pos; typeof=Cst.Null; is_used=true; is_assigned=false})
                        end
                    else ();
                    temp_scope_pointer
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method private get_id id = match id with
            | Cst.Id (x, _) ->
                    let temp_scope_pointer = self#get_containing_st_pointer id in
                    ref ((self#get_symbol_table !temp_scope_pointer)#get x)
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method assign id = match id with
            | Cst.Id (x, _) ->
                    (*
                     * Even if we didn't find the correct location, that's OK.
                     * The table will throw the error we want
                     *)
                    let temp_scope_pointer = self#get_containing_st_pointer id in
                    let symbol = !(self#get_id id) in
                    log_trace ("Assigning identifier " ^ (Char.escaped x));
                    (self#get_symbol_table !temp_scope_pointer)#set x {typeof=symbol.typeof; is_assigned=true; is_used=symbol.is_used; pos=symbol.pos}
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method use id = match id with
            | Cst.Id (x, _) ->
                    let temp_scope_pointer = self#get_containing_st_pointer id in
                    let symbol = !(self#get_id id) in
                    if
                        (not symbol.is_assigned)
                    then
                        log_warn ("Unassigned Variable being used on line " ^ (string_of_int symbol.pos.lineno))
                    else
                        ()
                    ;
                    log_trace ("Using identifier " ^ (Char.escaped x));
                    (self#get_symbol_table !temp_scope_pointer)#set x {typeof=symbol.typeof; is_assigned=symbol.is_assigned; is_used=true; pos=symbol.pos}
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method get_type_of id = match id with
            | Cst.Id (x, _) ->
                    let symbol = !(self#get_id id) in
                    symbol.typeof
            | _ -> raise IncorrectCSTElementsInSymbolTableError
        method warn_on_unused_in_current_scope =
            let table = self#get_symbol_table !current_scope in
            let symbols = table#all () in
            let unused_symbols = List.filter (fun (key, value) -> not value.is_used) symbols in
            List.iter (fun _ -> log_warn "Unused variable") unused_symbols
    end
;;

