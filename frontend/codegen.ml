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
