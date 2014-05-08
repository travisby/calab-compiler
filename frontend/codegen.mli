val assembly_list_of_ast : Ast.ast -> Symbol_table.symboltable -> Assembly.assembly list
val string_of_assembly_list : Assembly.assembly list -> string
val assemble : Assembly.assembly list -> Assembly.hex list
val string_of_hex_list : Assembly.hex list -> string
