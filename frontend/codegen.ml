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
let assembly_list_of_ast _ = Array.to_list (Array.make (max_address + 1) BRK)
let string_of_hex x = match x with
    | Hex x -> string_of_int x
let memory_address_as_string x = string_of_hex x
let constant_as_string x = string_of_hex x
let value_as_string x = match x with
    Memory_address x -> memory_address_as_string x
    | Constant x -> constant_as_string x
let rec string_of_hex_list hex_list = match hex_list with
    | [] -> ""
    (* this is the two-character version *)
    | Hex x :: xs when x >= 0xF -> "0x" ^ string_of_int x ^ " " ^ string_of_hex_list xs
    (* and this is the one character *)
    | Hex x :: xs -> "0x0" ^ string_of_int x ^ " " ^ string_of_hex_list xs
let rec string_of_assembly_list assembly_list = match assembly_list with
    | [] -> ""
    | LDA x :: xs -> "LDA " ^ value_as_string x ^ string_of_assembly_list xs
    | STA x :: xs -> "STA " ^ memory_address_as_string x ^ string_of_assembly_list xs
    | ADC x :: xs -> "ADC " ^ memory_address_as_string x ^ string_of_assembly_list xs
    | LDX x :: xs -> "LDX " ^ value_as_string x ^ string_of_assembly_list xs
    | LDY x :: xs -> "LDY " ^ value_as_string x ^ string_of_assembly_list xs
    | NOP :: xs -> "NOP " ^ string_of_assembly_list xs
    | BRK :: xs -> "BRK " ^ string_of_assembly_list xs
    | CPX x :: xs -> "CPX " ^ memory_address_as_string x ^ string_of_assembly_list xs
    | BNE x :: xs -> "BNE " ^ memory_address_as_string x ^ string_of_assembly_list xs
    | INC x :: xs -> "INC " ^ memory_address_as_string x ^ string_of_assembly_list xs
    | SYS :: xs -> "SYS " ^ string_of_assembly_list xs
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
