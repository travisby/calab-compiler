open Utils;;
open Assembly;;

exception Too_Large_Of_Program

let type_int = Ast.Int {charno=(-1); lineno=(-1)}
let type_bool = Ast.Boolean {charno=(-1); lineno=(-1)}
let type_string = Ast.String {charno=(-1); lineno=(-1)}
let a = 0
let x = 1
let y = 2
let true_string =
    let result = "truex" in
    String.set result 4 '\000';
    result
let false_string =
    let result = "falsex" in
    String.set result 5 '\000';
    result
(* No idea why + 1, but it works.... *)
let true_address = Assembly.max_address - (String.length true_string) + 1
let false_address = true_address - (String.length false_string)
let typeof ast st = match ast with
    | Ast.Id (x, pos) ->
            begin
                match st#get_type_of (Cst.Id (x, pos)) with
                    | Cst.Int _ -> type_int
                    | Cst.Boolean _ -> type_bool
                    | Cst.String _ -> type_string
                    | _ -> raise Not_found
            end
    | Ast.Addition _
        | Ast.Digit _
    -> type_int
    | Ast.Equallity_Test _
        | Ast.Inequallity_Test _
        | Ast.True _
        | Ast.False _
    -> type_bool
    | Ast.Char_List (xs, _) -> type_string
    | _ -> raise Not_found (* These do not have types *)
let to_char ast = match ast with
    | Ast.Char (x, _) -> x
    | _ -> raise Not_found
let assembly_list_of_ast ast st =
    let memory = Array.make (max_address + 1) BRK in
    let heap = Array.make (max_address + 1) BRK in
    let heap_pointer = ref max_address in
    let push_heap item =
        Array.set heap !heap_pointer item;
        heap_pointer := !heap_pointer - 1;
    in
    (* true *)
    push_heap (Data (Hex(0x00)));
    push_heap (Data (Hex(0x65)));
    push_heap (Data (Hex(0x75)));
    push_heap (Data (Hex(0x72)));
    push_heap (Data (Hex(0x74)));
    (* false *)
    push_heap (Data (Hex(0x00)));
    push_heap (Data (Hex(0x65)));
    push_heap (Data (Hex(0x73)));
    push_heap (Data (Hex(0x6c)));
    push_heap (Data (Hex(0x61)));
    push_heap (Data (Hex(0x66)));

    (*
     * NOTE
     *
     * Addition will overwrite the A register regardless of what you tell it to
     * due to architecture constraints.
     *
     * In/EquallityTest will overwrite both the A register and the X Register
     * due to our code generation plan
     *
     *)
    let rec func ?register:(register=a) ast = match ast with
        | Ast.Program (x, _) ->
                (* Program does what block does, except enters a new scope *)
                begin
                    match x with
                        | Ast.Block (xs, _) ->
                            List.flatten(List.map (fun x -> func x) xs)
                        | _ -> raise Not_found
                end
        | Ast.Block (xs, _) ->
                print_endline "Visiting child in ST";
                st#visit;
                let value = List.flatten(List.map func xs) in
                print_endline "Leaving child in ST";
                st#leave;
                value
        | Ast.Print_Statement (x, _) when typeof x st = type_int ->
            func ~register:y x @ [
                LDX(Constant(Hex(1)));
                SYS;
            ]
        | Ast.Print_Statement (x, _) when typeof x st = type_string ->
            func ~register:y x @ [
                LDX(Constant(Hex(2)));
                Reserved;
                SYS;
            ]
        | Ast.Print_Statement (x, _) when typeof x st = type_bool ->
            begin
                match x with
                    | Ast.True _ ->
                        [
                            LDY(Constant(Hex(true_address)));
                            Reserved;
                            LDX(Constant(Hex(2)));
                            Reserved;
                            SYS;
                        ]
                    | Ast.False _ ->
                        [
                            LDY(Constant(Hex(false_address)));
                            Reserved;
                            LDX(Constant(Hex(2)));
                            Reserved;
                            SYS;
                        ]
                    | Ast.Equallity_Test _
                        | Ast.Inequallity_Test _
                    ->
                        func ~register:y x @ [
                            LDX(Constant(Hex(2)));
                            Reserved;
                            SYS;
                        ]
                    | _ -> raise Not_found
            end
        | Ast.Assignment_Statement (x, y, _) ->
            func y ~register:a @ [
                STA(st#get_address x);
                Reserved;
                Reserved;
            ]
        | Ast.Var_Decl (x, y, _) ->
            st#reserve_static_space y;
            []
        (* Overwrites X, A register *)
        | Ast.While_Statement (bool_expr, block, pos) ->
            let compare = func ~register:x bool_expr in
            let stmts = func block in
            compare @ [
                CPX(Hex(true_address)); Reserved; Reserved;
                (* + 5 for the unconditional jump *)
                BNE(Hex((List.length stmts) + 5)); Reserved
            ]
            @ stmts
            (* load true for an unconditional jump to the top of the while *)
            @ func ~register:x (Ast.True pos)
            @ [
                CPX(Hex(true_address)); Reserved; Reserved;
                (* -2 for the CPX + BNE for the loop *)
                BNE(Hex(max_address - (List.length stmts) - 2 - (List.length compare))); Reserved
            ]

        (* Overwrites X, A register *)
        | Ast.If_Statement (bool_expr, block, _) ->
            let compare = func ~register:x bool_expr in
            let stmts = func block in
            compare @ [
                CPX(Hex(true_address)); Reserved; Reserved;
                BNE(Hex(List.length stmts)); Reserved
            ]
            @ stmts
        | Ast.Id _ ->
            [
                if
                    register = a
                then
                    LDA(Memory_address(st#get_address ast))
                else begin
                    if
                        register = x
                    then
                        LDY(Memory_address(st#get_address ast))
                    else(* register = y *)
                        LDX(Memory_address(st#get_address ast))
                end;
                Reserved;
                Reserved;
        ]
        | Ast.Addition (digit, expr, _) ->
                (* NOTICE: WIPES A REGISTER *)
                (func ~register:a digit)
                @ [STA(st#get_temp_address); Reserved; Reserved]
                @ (func ~register: a expr)
                @ [ADC(st#get_temp_address); Reserved; Reserved]
                @ if
                    register = a
                then []
                else begin
                    if
                        register = x
                    then begin
                        [
                            STA(st#get_temp_address);
                            Reserved;
                            Reserved;
                            LDX(Memory_address(st#get_temp_address));
                            Reserved;
                            Reserved;
                        ]
                    end else begin (* register = y *)
                        [
                            STA(st#get_temp_address);
                            Reserved;
                            Reserved;
                            LDY(Memory_address(st#get_temp_address));
                            Reserved;
                            Reserved;
                        ]
                    end
                end
        (*
         * Equallity test works by loading the register with either true, or
         * false address.  NOT by setting the Z Flag (ultimately)
         * Anything using the equality test should BNE on address = false
         *
         * The ZFlag is not the best way to do this because d == (true != false)
         * would have the z flag only returning the last possible result,
         * because it cannot be read.  We would need some form of lookahead in
         * codegen to utilize it.  This produces less code overall for
         * complicated expressions
         *
         * OVERWRITES A, X
         *)
        | Ast.Equallity_Test (expr1, expr2, _) ->
                (* Evaluate expr1 and store into temp *)
                func ~register:a expr1
                @ [STA(st#get_temp_address); Reserved; Reserved]
                (* Evaluate expr2 into register x *)
                @ func ~register:x expr2
                (* Run our comparison early, so we can wipe registers *)
                @ [CPX(st#get_temp_address); Reserved; Reserved]
                (* Save false into ~register *)
                @ [
                    if
                        register = a
                    then
                        LDA(Constant(Hex(false_address)))
                    else begin
                        if
                            register = x
                        then
                            LDX(Constant(Hex(false_address)))
                        else
                            LDY(Constant(Hex(false_address)))
                    end;
                    Reserved
                ]
                (* If we are false, skip over... "temp = true" *)
                @ [BNE(Hex(5)); Reserved]
                (* If we are true, do "~register = true" *)
                @ [
                    if
                        register = a
                    then
                        LDA(Constant(Hex(true_address)))
                    else begin
                        if
                            register = x
                        then
                            LDX(Constant(Hex(true_address)))
                        else
                            LDY(Constant(Hex(true_address)))
                    end;
                    Reserved
                ]
        | Ast.Inequallity_Test (expr1, expr2, _) ->
                (* Evaluate expr1 and store into temp *)
                func ~register:a expr1
                @ [STA(st#get_temp_address); Reserved; Reserved]
                (* Evaluate expr2 into register x *)
                @ func ~register:x expr2
                (* Run our comparison early, so we can wipe registers *)
                @ [CPX(st#get_temp_address); Reserved; Reserved]
                (* Save true into ~register *)
                @ [
                    if
                        register = a
                    then
                        LDA(Constant(Hex(true_address)))
                    else begin
                        if
                            register = x
                        then
                            LDX(Constant(Hex(true_address)))
                        else
                            LDY(Constant(Hex(true_address)))
                    end;
                    Reserved
                ]
                (* If we are false, skip over... "temp = false" *)
                @ [BNE(Hex(5)); Reserved]
                (* If we are true, do "~register = false" *)
                @ [
                    if
                        register = a
                    then
                        LDA(Constant(Hex(false_address)))
                    else begin
                        if
                            register = x
                        then
                            LDX(Constant(Hex(false_address)))
                        else
                            LDY(Constant(Hex(false_address)))
                    end;
                    Reserved
                ]
        | Ast.Char (x, _) -> (* This case shouldn't happen.  It's handled in CharList *) raise Not_found
        | Ast.Digit (d, _) ->
            [
                if
                    register = a
                then
                    LDA(Constant(Hex(d)))
                else begin
                    if
                        register = x
                    then
                        LDX(Constant(Hex(d)))
                    else
                        LDY(Constant(Hex(d)))
                end
            ] @ [Reserved]
        | Ast.True _ ->
            [
                if
                    register = a
                then
                    LDA(Constant(Hex(true_address)))
                else begin
                    if register = x
                    then
                        LDX(Constant(Hex(true_address)))
                    else
                        LDY(Constant(Hex(true_address)))
                end
            ] @ [Reserved]
        | Ast.False _ ->
            [
                if
                    register = a
                then
                    LDA(Constant(Hex(false_address)))
                else begin
                    if register = x
                    then
                        LDX(Constant(Hex(false_address)))
                    else
                        LDY(Constant(Hex(false_address)))
                end
            ] @ [Reserved]
        | Ast.Char_List (xs, _) ->
            (* Pay no attention to the code cruft behind the curtain *)
            (*
             * There is no reason other than "Oh man I need to get this done"
             * for having the symbol table track whether or not something is in
             * the heap
             *)
            let str = (String.concat "" (List.map (fun x -> Char.escaped (to_char x)) xs)) in
            if
                st#is_in_heap str
            then begin
                (* null *)
                push_heap(Data(Hex(0x00)));
                (* rest of the string *)
                List.iter (fun x -> push_heap (Data(Hex(Char.code (to_char x))))) (List.rev xs)
            end else
                ()
            ;
            [
                if
                    register = a
                then
                    LDA(Constant(Hex(st#get_heap_address str)))
                else begin
                    if register = x
                    then
                        LDX(Constant(Hex(st#get_heap_address str)))
                    else
                        LDY(Constant(Hex(st#get_heap_address str)))
                end
            ] @ [Reserved]
        | _ -> raise Not_found
    in
    let instructions = func ast in
    (* update pointers *)
    st#update_static_addresses  ((List.length instructions) + 1);
    let static = Array.to_list (Array.make st#sizeof_static BRK) in 
    (* Make sure we're not too big of a program *)
    if
        ((List.length instructions) + (List.length static) + (max_address - !heap_pointer)) > max_address
    then
        raise Too_Large_Of_Program
    else ()
    ;
    List.iteri (fun i x -> Array.set memory i x) (instructions @ static);
    (* add all memory of heap that's... past the heap pointer *)
    Array.iteri (fun i x -> if i >= !heap_pointer then Array.set memory i x else ()) heap;
    Array.to_list memory
let string_of_hex x = match x with
    | Hex x -> string_of_int x
    | Temp_Hex x -> string_of_int !x
let string_of_memory_address x =
    (* Ensure the usage of two bytes *)
    let res = string_of_hex x in
    if String.length res <=2
    then 
        "$" ^ res ^ " 00"
    else 
        "$" ^ res
let string_of_constant x = "#$" ^ string_of_hex x
let string_of_value x = match x with
    Memory_address x -> string_of_memory_address x
    | Constant x -> string_of_constant x
let rec string_of_hex_list hex_list =
    (* http://stackoverflow.com/a/10440025/868465 *)
    let string_of_int = Printf.sprintf "%X" in
    match hex_list with
    | [] -> ""
    (* this is the two-character version *)
    | Hex x :: xs when x >= 0xF -> "" ^ string_of_int x ^ " " ^ string_of_hex_list xs
    (* and this is the one character *)
    | Hex x :: xs -> "0" ^ string_of_int x ^ " " ^ string_of_hex_list xs
    | Temp_Hex x :: xs -> string_of_hex_list (Hex !x :: xs)
let rec string_of_assembly_list assembly_list =
    let string_of_instruction instruction = match instruction with
        | LDA x -> "LDA " ^ string_of_value x
        | STA x -> "STA " ^ string_of_memory_address x
        | ADC x -> "ADC " ^ string_of_memory_address x
        | LDX x -> "LDX " ^ string_of_value x
        | LDY x -> "LDY " ^ string_of_value x
        | NOP -> "NOP"
        | BRK -> "BRK"
        | CPX x -> "CPX " ^ string_of_memory_address x
        | BNE x -> "BNE " ^ string_of_memory_address x
        | INC x -> "INC " ^ string_of_memory_address x
        | SYS -> "SYS"
        | Data x -> string_of_hex x
        | Reserved -> ""
        | Temp x -> string_of_hex x
    in String.concat " " (List.map string_of_instruction assembly_list)
let rec assemble assembly_list =
    let needs_zero hex = match hex with
        | Hex x -> x < 0xFF
        | Temp_Hex x -> !x < 0xFF
    in
    let assemble_one instruction = match instruction with
        | LDA (Constant x) -> [Hex 0xA9; x]
        | LDA (Memory_address x) -> [Hex 0xAD; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | STA x -> [Hex 0x8D; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | ADC x -> [Hex 0x6D; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | LDX (Constant x) -> [Hex 0xA2; x]
        | LDX (Memory_address x) -> [Hex 0xAE; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | LDY (Constant x) -> [Hex 0xA0; x]
        | LDY (Memory_address x) -> [Hex 0xAC; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | NOP -> [Hex 0xEA]
        | BRK -> [Hex 0x00]
        | CPX x -> [Hex 0xEC; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | BNE x -> [Hex 0xEF; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | INC x -> [Hex 0xEE; x] @ (if needs_zero(x) then [Hex 0x00] else [])
        | SYS -> [Hex 0xFF]
        | Data x -> [x]
        | Temp x -> [x]
        | Reserved -> []
    in
    List.flatten (List.map assemble_one assembly_list)
