exception Already_Exists_In_table;;
exception CannotExitGlobalScope;;
exception IncorrectCSTElementsInSymbolTableError;;
exception Does_Not_Exist_In_Table;;

class ['a, 'b] table =
    object (self)
        val mutable t = Hashtbl.create 0
        method mem x = Hashtbl.mem t x
        method get x = Hashtbl.find t x
        method add (x:'a) (y:'b) =
            if
                self#mem x
            then
                raise Already_Exists_In_table
            else
                Hashtbl.add t x y
        method set (x: 'a) (y: 'b) =
            if
                not (self#mem x)
            then
                raise Does_Not_Exist_In_Table
            else
                Hashtbl.replace t x y
    end
;;

(*
 * We use an array because it is a mutable data structure.
 * We will be appending to the array to "add" new scopes
 *)
type scope =
    | Global of (char, Cst.cst) table * scope array
    (* The last parameter is the parent scope *)
    | Scope of (char, Cst.cst) table * scope array * scope ref
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
        (* TODO worry about type *)
        method add id _type = match id, _type with
            | Cst.Id x, Cst.Int -> !(self#get_current_table)#add x Cst.Null
            | Cst.Id x, Cst.Boolean -> !(self#get_current_table)#add x Cst.Null
            | Cst.Id x, Cst.String -> !(self#get_current_table)#add x Cst.Null
            | _, _ -> raise IncorrectCSTElementsInSymbolTableError
        method set id _val = match id with
            | Cst.Id x ->
                    let get_parent scope = match scope with
                        | Scope (_, _, x) -> x
                        | Global (_, _) -> raise CannotExitGlobalScope
                    in
                    let get_symbol_table scope = match scope with
                        | Scope (t, _, _) -> t
                        | Global (t, _) -> t
                    in
                    (* reference to the dereference just allows us to have a mutable property *)
                    let temp_scope_pointer = ref !current_scope in
                    (*
                     * If temp_score_pointer = head, then we are at the global
                     * scope.  Let it error here if it wants.  Also stop if we
                     * the value exists here!
                     *)
                    while (temp_scope_pointer <> head && not ((get_symbol_table !temp_scope_pointer)#mem x)) do
                        (* ocaml will only take a dereference... *)
                        temp_scope_pointer := !(get_parent !temp_scope_pointer)
                    done;
                    (*
                     * Even if we didn't find the correct location, that's OK.
                     * The table will throw the error we want
                     *)
                    (get_symbol_table !temp_scope_pointer)#set x _val
            | _ -> raise IncorrectCSTElementsInSymbolTableError
    end
;;

