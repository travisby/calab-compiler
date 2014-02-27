type program = Program of block * dollarSign
and block = Block of open_brace * statement_list * close_brace
and statement_list =
    | Emtpy_StatementList
    | Statement_List of statement * statement_list
and statement =
    | Statement_Print_Statement of print_statement
    | Statement_Assignment_Statement of assignment_statement
    | Statement_Var_Decl of var_decl
    | Statement_While_Statement of while_statement
    | Statement_If_Statement of if_statement
    | Statement_Block of block
and print_statement = Print_Statement of open_paren * expr * close_paren
and assignment_statement = Assignment_Statement of id * equals * expr
and var_decl = Var_Decl of _type * id
and while_statement = While_Statement of _while * boolean_expr * block
and if_statement = If_Statement of _if * boolean_expr * block
and expr =
    | Expr_Int_Expr of int_expr
    | Expr_String_Expr of string_expr
    | Expr_id of id
and int_expr = Int_Expr of digit * intop * digit
and string_expr = String_Expr of quote * char_list * quote
and boolean_expr = Boolean_Expr of open_paren * expr * boolop * expr * close_paren
and id = Id of _char
and char_list =
    | CharList of _char * char_list
    | SpacedCharList of space * char_list
    | Empty_Char_List
and _type =
    | Int | String | Boolean
and _char =
    | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
and space =
    | Space
and digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
and boolop =
    | Equal
    | NotEqual
and boolval =
    | False
    | True
and intop =
    | Plus
and dollarSign = | DollarSign
and open_brace = | OpenBrace
and close_brace = | CloseBrace
and open_paren = | OpenParen
and close_paren = | CloseParen
and equals = | Equals
and _while = | While
and _if = | If
and quote = Quote
;;

(* oop queue *)
class ['a] queue (qu) =
    object
        (* note, this is actually mutable! *)
        val mutable q = (qu : 'a)
        method peek = List.hd q
        (*
         * So what's happening here is actually pretty cool
         * let $x = $y in $z will return $y to the caller of $x, then calculate
         * the () returnd $z
         *)
        method pop = let f = (List.hd q) in q <- (List.tl q); f
        method get_list = q
    end
;;

(* possible tokens... T_OpenBrace, T_CloseBrace, T_print, T_OpenParen,
 * T_CloseParen, T_while, T_id, T_if, T_int, T_string, T_boolean, T_char,
 * T_digit, T_assignment, T_equality, T_inequality, T_false, T_true, T_plus,
 * T_doublequote
 *)
