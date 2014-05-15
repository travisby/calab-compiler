type hex =
    Hex of int
    | Temp_Hex of int ref
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
    | Data of hex (* used for straight data in heap *)
    | Temp of hex
    | Reserved (* Used after two-byte commands to reserve space *)
let max_address = 0xff
