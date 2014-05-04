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
val assembly_list_of_ast : Ast.ast -> assembly list
val string_of_assembly_list : assembly list -> string
val assemble : assembly list -> hex list
val string_of_hex_list : hex list -> string
