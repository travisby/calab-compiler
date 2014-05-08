open Utils;;

type hex = Hex of int
type memory_address = hex
type constant = hex
type value = 
    Memory_address of memory_address
    | Constant of constant
type assembly =
    | LDA of value
    | STA of memory_address
    | ADC of memory_address
    | LDX of value
    | LDY of value
    | NOP
    | BRK
    | CPX of memory_address
    | BNE of memory_address
    | INC of memory_address
    | SYS
let max_address = 0xFF
let type_int = Ast.Int {charno=(-1); lineno=(-1)}
let type_bool = Ast.Boolean {charno=(-1); lineno=(-1)}
let type_string = Ast.String {charno=(-1); lineno=(-1)}
let true_string =
    let result = "truex" in
    String.set result 4 '\000';
    result
let false_string =
    let result = "falsex" in
    String.set result 5 '\000';
    result
let true_address = max_address - (String.length true_string)
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

let assembly_list_of_ast ast st =
    let memory = Array.make max_address BRK in
    let rec func ast = match ast with
        | Ast.Program (x, _) -> func x
        | Ast.Block (xs, _) ->
                st#visit;
                let value = List.flatten(List.map func xs) in
                st#leave;
                value
        | Ast.Print_Statement (x, _) when typeof x st = type_int ->
            func x @ [
                LDY(Constant(Hex(1)));
                SYS;
            ]
        | Ast.Print_Statement (x, _) when typeof x st = type_string ->
            func x @ [
                LDY(Constant(Hex(2)));
                SYS;
            ]
        | Ast.Print_Statement (x, _) when typeof x st = type_bool ->
            begin
                match x with
                    | Ast.True _ ->
                        [
                            LDX(Memory_address(Hex(true_address)));
                            LDY(Constant(Hex(2)));
                            SYS;
                        ]
                    | Ast.False _ ->
                        [
                            LDX(Memory_address(Hex(false_address)));
                            LDY(Constant(Hex(2)));
                            SYS;
                        ]
                    | Ast.Equallity_Test _
                        | Ast.Inequallity_Test _
                    ->
                        func x @ [
                            LDY(Constant(Hex(2)));
                            SYS;
                        ]
            end
        | Ast.Assignment_Statement (x, y, _) ->
            (* TODO this for different types for x *)
            func y @ [
                STA(st#get_address x)
            ]
        | Ast.Var_Decl (x, y, _) -> []
        | Ast.While_Statement (x, y, _) -> raise Not_found
        | Ast.If_Statement (x, y, _) -> raise Not_found
        | Ast.Id (x, _) ->
            [
                (* brute force solution *)
                (* TODO have an optional parameter saying where to save things *)
                LDA(Memory_address(st#get_address ast));
                LDY(Memory_address(st#get_address ast));
                LDX(Memory_address(st#get_address ast));
        ]
        | Ast.Addition (x, y, _) -> raise Not_found
        | Ast.Equallity_Test (x, y, _) -> raise Not_found
        | Ast.Inequallity_Test (x, y, _) -> raise Not_found
        | Ast.Char (x, _) -> raise Not_found
        | Ast.Digit (d, _) ->
            [
                LDA(Constant(Hex(d)));
                LDX(Constant(Hex(d)));
                LDY(Constant(Hex(d)));
            ]
        | Ast.True _ ->
            [
                LDA(Memory_address(Hex(true_address)));
                LDX(Memory_address(Hex(true_address)));
                LDY(Memory_address(Hex(true_address)));
            ]
        | Ast.False _ ->
            [
                LDA(Memory_address(Hex(false_address)));
                LDX(Memory_address(Hex(false_address)));
                LDY(Memory_address(Hex(false_address)));
            ]
        | Ast.Char_List (xs, _) ->
            [
                LDA(Memory_address(st#get_address ast));
                LDY(Memory_address(st#get_address ast));
                LDX(Memory_address(st#get_address ast));
            ]
    in 
    Array.to_list memory
let string_of_hex x = match x with
    | Hex x -> string_of_int x
let string_of_memory_address x = string_of_hex x
let string_of_constant x = string_of_hex x
let string_of_value x = match x with
    Memory_address x -> string_of_memory_address x
    | Constant x -> string_of_constant x
let rec string_of_hex_list hex_list = match hex_list with
    | [] -> ""
    (* this is the two-character version *)
    | Hex x :: xs when x >= 0xF -> "0x" ^ string_of_int x ^ " " ^ string_of_hex_list xs
    (* and this is the one character *)
    | Hex x :: xs -> "0x0" ^ string_of_int x ^ " " ^ string_of_hex_list xs
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
    in String.concat " " (List.map string_of_instruction assembly_list)
let rec assemble assembly_list =
    let assemble_one instruction = match instruction with
        | LDA (Constant x) -> [Hex 0xA9; x]
        | LDA (Memory_address x) -> [Hex 0xAD; x]
        | STA x -> [Hex 0x8D; x]
        | ADC x -> [Hex 0x6D; x]
        | LDX (Constant x) -> [Hex 0xA2; x]
        | LDX (Memory_address x) -> [Hex 0xAE; x]
        | LDY (Constant x) -> [Hex 0xA0; x]
        | LDY (Memory_address x) -> [Hex 0xAC; x]
        | NOP -> [Hex 0xEA]
        | BRK -> [Hex 0x00]
        | CPX x -> [Hex 0xEC; x]
        | BNE x -> [Hex 0xEF; x]
        | INC x -> [Hex 0xEE; x]
        | SYS -> [Hex 0xFF]
    in
    List.flatten (List.map assemble_one assembly_list)
