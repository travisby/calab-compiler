exception Expected_Something_Else of string * Lex.token
type program = Program of block * dollar_sign
and block = Block of open_brace * statement_list * close_brace
and statement_list =
    Emtpy_Statement_List
  | Statement_List of statement * statement_list
and statement =
    Statement_Print_Statement of print_statement
  | Statement_Assignment_Statement of assignment_statement
  | Statement_Var_Decl of var_decl
  | Statement_While_Statement of while_statement
  | Statement_If_Statement of if_statement
  | Statement_Block of block
and print_statement = Print_Statement of open_paren * expr * close_paren
and assignment_statement = Assignment_Statement of id * equals * expr
and var_decl = Var_Decl of _type * id
and while_statement = While_Statement of boolean_expr * block
and if_statement = If_Statement of boolean_expr * block
and expr =
    Expr_Int_Expr of int_expr
  | Expr_String_Expr of string_expr
  | Expr_Id_Expr of id
and int_expr = Int_Expr of digit * intop * digit
and string_expr = String_Expr of quote * char_list * quote
and boolean_expr =
    Boolean_Expr of open_paren * expr * boolop * expr * close_paren
and id = Id of _char
and char_list = Char_List of string | Empty_Char_List
and _type = Int | String | Boolean
and _char = Char of char
and space = Space
and digit = Digit of int
and boolop = Equal | Not_Equal
and boolval = False | True
and intop = Plus
and dollar_sign = Dollar_Sign
and open_brace = Open_Brace
and close_brace = Close_Brace
and open_paren = Open_Paren
and close_paren = Close_Paren
and equals = Equals
and _while = While
and _if = If
and quote = Quote
val parse : Lex.token list -> program
