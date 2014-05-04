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
    | Hex x :: xs -> string_of_int x ^ " " ^ string_of_hex_list xs
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
        (* TODO *)
        | _ -> Hex 0x00
    in
    List.map assemble_one assembly_list
