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
    let global_table = ref (new table) in
    let scope_init = Global (!global_table, [||]) in
    object (self)
        val mutable head = ref scope_init
        val mutable current_scope = ref scope_init
        val mutable symbol_tables = [[global_table]]
        (* not proud of this...
         * if there are any bugs, this is probably the first place to look
         * 5 = true \0
         * 6 = false \0
         *)
        val mutable pseudo_heap_pointer = 0xFF - 5 - 6
        val mutable static_pointer = 0
        val heap = Hashtbl.create 2
        val static = Hashtbl.create 0
        val mutable visiting_node = 0
        method sizeof_static = Hashtbl.length static
        (* add start to every value *)
        method update_static_addresses start =
            Hashtbl.iter (fun key value -> value := (start + !value)) static;
        method reserve_static_space ast =
            Hashtbl.add static (self#get_id_ast ast) (ref static_pointer);
            static_pointer <- static_pointer + 1;
        method is_in_heap (str : string) =
            if
                Hashtbl.mem heap str
            then
                true
            else begin
                (* subtract the space from our terrible fake pointer to the heap *)
                (* the + 1 is for \0 *)
                pseudo_heap_pointer <- pseudo_heap_pointer - ((String.length str) + 1);
                Hashtbl.add heap str pseudo_heap_pointer;
                false
            end
        method get_heap_address (str : string) = (Hashtbl.find heap str) + 1
        method private get_current_table = match !current_scope with
                | Scope (t, _, _) -> ref t
                | Global (t, _) -> ref t
        method get_address (ast : Ast.ast) =
            let res = Assembly.Temp_Hex((Hashtbl.find static (self#get_id_ast ast))) in
            res
        method get_temp_address = Assembly.Hex(0x00)
        method leave =
            ()
        method visit =
            visiting_node <- visiting_node + 1
        method enter =
            (*
             * We seem (because we actually aren't) to be abusing let..in
             * statements, but it enforces execution order for us, which is not
             * guaranteed by the Ocaml language.  For all of the stuff depending
             * on state (say... moving the scope pointer!) it's important to
             * order the execution of statements
             *)
            let new_scope = ref (scope_constructor current_scope) in
            let table = match !new_scope with
                | Scope (table, _, _) -> table
                | Global (table, _) -> table
            in
            let rec recur scope =
                match scope with
                    | Global (table, _) -> [ref table]
                    | Scope (table, _, parent) -> ref table :: (recur !parent)
            in
            let parent_scopes = recur !current_scope in
            (* Add the new scope! *)
            symbol_tables <- (symbol_tables @ [ref table :: parent_scopes]);
            current_scope <- new_scope
        method exit =
            let parent_scope = match !current_scope with
                | Scope (_, _, x) -> x
                | Global (_, _) -> raise CannotExitGlobalScope
            in
            current_scope <- parent_scope;
            let table = match !current_scope with
                | Scope (table, _, _) -> table
                | Global (table, _) -> table
            in
            let rec recur scope =
                match scope with
                    | Global (table, _) -> [ref table]
                    | Scope (table, _, parent) -> ref table :: (recur !parent)
            in
            let parent_scopes = recur !current_scope in
            (* Add the new scope! *)
            symbol_tables <- (symbol_tables @ [ref table :: parent_scopes]);
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
        method private get_id_ast id = match id with
            | Ast.Id (x, y) ->
                    let keys_and_table_product =
                        let rec range i j = if i > j then [] else i :: (range (i+1) j) in
                        List.flatten
                            (
                                List.map
                                    (fun st -> List.map (fun v -> ((x, v), st)) (List.rev (range 0 visiting_node)))
                                    (List.nth symbol_tables visiting_node)
                        )
                    in
                    let containing_tables = (List.filter (fun ((k,j), table) -> !table#mem x) keys_and_table_product) in
                    let key,table = List.hd (containing_tables) in
                    let res = (x, fst(key)) in
                    res
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

