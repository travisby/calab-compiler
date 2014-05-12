open Utils;;
open Assembly;;

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
let true_address = Assembly.max_address - (String.length true_string)
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

let assembly_list_of_ast ast st =
    let memory = Array.make max_address BRK in
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
                            LDY(Memory_address(Hex(true_address)));
                            Reserved;
                            Reserved;
                            LDX(Constant(Hex(2)));
                            Reserved;
                            SYS;
                        ]
                    | Ast.False _ ->
                        [
                            LDY(Memory_address(Hex(false_address)));
                            Reserved;
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
            (* TODO this for different types for x *)
            func y @ [
                STA(st#get_address x);
                Reserved;
                Reserved;
            ]
        | Ast.Var_Decl (x, y, _) -> []
        | Ast.While_Statement (x, y, _) -> raise Not_found
        | Ast.If_Statement (x, y, _) -> raise Not_found
        | Ast.Id (x, _) ->
            [
                (* brute force solution *)
                (* TODO have an optional parameter saying where to save things *)
                LDA(Memory_address(st#get_address ast));
                Reserved;
                Reserved;
                LDY(Memory_address(st#get_address ast));
                Reserved;
                Reserved;
                LDX(Memory_address(st#get_address ast));
                Reserved;
                Reserved;
        ]
        | Ast.Addition (digit, expr, _) ->
                (func ~register: a digit)
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
        | Ast.Equallity_Test (x, y, _) -> raise Not_found
        | Ast.Inequallity_Test (x, y, _) -> raise Not_found
        | Ast.Char (x, _) -> raise Not_found
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
                    LDA(Memory_address(Hex(true_address)))
                else begin
                    if register = x
                    then
                        LDX(Memory_address(Hex(true_address)))
                    else
                        LDY(Memory_address(Hex(true_address)))
                end
            ] @ [Reserved; Reserved]
        | Ast.False _ ->
            [
                if
                    register = a
                then
                    LDA(Memory_address(Hex(false_address)))
                else begin
                    if register = x
                    then
                        LDX(Memory_address(Hex(false_address)))
                    else
                        LDY(Memory_address(Hex(false_address)))
                end
            ] @ [Reserved; Reserved]
        | Ast.Char_List (xs, _) ->
            [
                LDA(Memory_address(st#get_address ast));
                Reserved; Reserved;
                LDY(Memory_address(st#get_address ast));
                Reserved; Reserved;
                LDX(Memory_address(st#get_address ast));
                Reserved; Reserved;
            ]
        | _ -> raise Not_found
    in
    List.iteri (fun i x -> Array.set memory i x) (func ast);
    Array.to_list memory
let string_of_hex x = match x with
    | Hex x -> string_of_int x
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
        | Reserved -> ""
    in String.concat " " (List.map string_of_instruction assembly_list)
let rec assemble assembly_list =
    let needs_zero hex = match hex with
        | Hex x -> x < 0xFF
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
        | Reserved -> []
    in
    List.flatten (List.map assemble_one assembly_list)
