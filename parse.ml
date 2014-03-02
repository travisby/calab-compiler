exception Expected_Something_Else of string * Lex.token;;

exception Expected_Something_Else of string * token;;

type program = Program of block * dollarSign
and block = Block of open_brace * statement_list * close_brace
and statement_list =
    | Emtpy_Statement_List
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
and while_statement = While_Statement of boolean_expr * block
and if_statement = If_Statement of boolean_expr * block
and expr =
    | Expr_Int_Expr of int_expr
    | Expr_String_Expr of string_expr
    | Expr_Id_Expr of id
and int_expr = Int_Expr of digit * intop * digit
and string_expr = String_Expr of quote * char_list * quote
and boolean_expr = Boolean_Expr of open_paren * expr * boolop * expr * close_paren
and id = Id of _char
and char_list =
    | Char_List of string
    | Empty_Char_List
and _type =
    | Int | String | Boolean
and _char =
    | Char of char
and space =
    | Space
and digit = Digit of int
and boolop =
    | Equal
    | Not_Equal
and boolval =
    | False
    | True
and intop =
    | Plus
and dollarSign = | Dollar_Sign
and open_brace = | Open_Brace
and close_brace = | Close_Brace
and open_paren = | Open_Paren
and close_paren = | Close_Paren
and equals = | Equals
and _while = | While
and _if = | If
and quote = Quote
;;

(* oop LL(1) queue *)
class ['a] queue (qu) =
    object
        (* note, this is actually mutable! *)
        val mutable q = (qu : 'a)
        method peek = List.hd q
        method look_ahead_1 = List.hd (List.tl q)
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

(*
 * non-terminals peek, terminals pop
 *)
let rec parse tokens = parse_program (new queue tokens)
and parse_program tokens =
    let block = parse_block tokens in
    let dollar_sign = parse_dollar_sign tokens in
    Program (block, dollar_sign)
 (*
  * because we join with "and" and a previously recursive function, this is also recursive
  * We use the let .. in syntax to force order of evaluation.  OCaml makes
    no guarantees over the order of execution of parameters
    http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora029.html 
  *)
and parse_block tokens =
    (* these are analogous to local variables *)
    let ob = parse_open_brace tokens in
    let statement_list = parse_statement_list tokens in
    let cb = parse_close_brace tokens in
    (* this is the return value *)
    Block (ob, statement_list, cb)
and parse_open_brace tokens = match tokens#pop with
    | Lex.T_Open_Brace _ -> Open_Brace
    | x -> raise (Expected_Something_Else("Open Brace", x))
and parse_close_brace tokens = match tokens#pop with
    | Lex.T_Close_Brace _ -> Close_Brace
    | x -> raise (Expected_Something_Else("Close Brace", x))

(*
 * We must assume the parse_statement into this function, or else we wouldn't
 * know whether to issue an empty statement list, or an unexpected token error
 *)
and parse_statement_list tokens = match tokens#peek with
    | Lex.T_Print x ->
            let print_statement = parse_print_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Print_Statement (print_statement), statement_list)
    | Lex.T_Id x ->
            let assignment_statement = parse_assignment_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Assignment_Statement (assignment_statement), statement_list)
    (*
    | T_Int x -> Statement_List (Statement_Var_Decl (parse_var_decl_statement tokens), parse_statement_list tokens)
    | T_String x -> Statement_List (Statement_Var_Decl (parse_var_decl_statement tokens), parse_statement_list tokens)
    | T_Boolean x -> Statement_List (Statement_Var_Decl (parse_var_decl_statement tokens), parse_statement_list tokens)
    *)
    | Lex.T_While x ->
            let while_statement = parse_while_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_While_Statement (while_statement), statement_list)
    | Lex.T_If x ->
            let if_statement = parse_if_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_If_Statement (if_statement), statement_list)
    | Lex.T_Open_Brace x ->
            let block = parse_block tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Block (block), statement_list)
    | _ -> Emtpy_Statement_List
and parse_print_statement tokens = match tokens#pop with
    | Lex.T_Print _ ->
            let op = parse_open_paren tokens in
            let expr = parse_expr tokens in
            let cp = parse_close_paren tokens in
            Print_Statement (op, expr, cp)
    | x -> raise (Expected_Something_Else("Print", x))
and parse_assignment_statement tokens = 
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    let equals = parse_equals tokens in
    let expr = parse_expr tokens in
    Assignment_Statement (_id, equals, expr)
and parse_var_decl_statement tokens =
    (* remember, type is a reserved word, and _type  is already used as a type! *)
    let the_type = parse_type tokens in
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    Var_Decl (the_type, _id)
and parse_while_statement tokens = match tokens#pop with
    | Lex.T_While _ ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            While_Statement (boolean_expr, block)
    | x -> raise (Expected_Something_Else("While", x))
and parse_if_statement tokens = match tokens#pop with
    | Lex.T_If _ ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            If_Statement (boolean_expr, block)
    | x -> raise (Expected_Something_Else("If", x))
and parse_open_paren tokens = match tokens#pop with
    | Lex.T_Open_Paren _ -> Open_Paren
    | x -> raise (Expected_Something_Else("Open Paren", x))
and parse_expr tokens = match tokens#peek with
    | Lex.T_Digit _ -> Expr_Int_Expr (parse_int_expr tokens)
    | Lex.T_Double_Quote _ -> Expr_String_Expr (parse_string_expr tokens)
    | Lex.T_Id _ -> Expr_Id_Expr (parse_id tokens)
    | x -> raise (Expected_Something_Else("Digit | Quote | Id", x))
and parse_close_paren tokens = match tokens#pop with
    | Lex.T_Close_Paren _ -> Close_Paren
    | x -> raise (Expected_Something_Else("Close Paren", x))
and parse_id tokens = match tokens#peek with 
    | Lex.T_Id x -> Id (parse_char tokens)
    | x -> raise (Expected_Something_Else("Id", x))
and parse_equals tokens = match tokens#pop with
    | Lex.T_Assignment _ -> Equals
    | x -> raise (Expected_Something_Else("=", x))
and parse_type tokens = match tokens#pop with
    | Lex.T_Int _ -> Int
    | Lex.T_Boolean _ -> Boolean
    | Lex.T_String _ -> String
    | x -> raise (Expected_Something_Else("int | boolean | string", x))
and parse_boolean_expr tokens =
    let op = parse_open_paren tokens in
    let expr1 = parse_expr tokens in
    let boolop = parse_boolop tokens in
    let expr2 = parse_expr tokens in
    let cp = parse_close_paren tokens in
    Boolean_Expr (op, expr1, boolop, expr2, cp)
and parse_int_expr tokens =
    let digit1 = parse_digit tokens in
    let intop = parse_intop tokens in
    let digit2 = parse_digit tokens in
    Int_Expr (digit1, intop, digit2)
and parse_string_expr tokens =
    let quote1 = parse_quote tokens in
    let char_list = parse_char_list tokens in
    let quote2 = parse_quote tokens in
    String_Expr (quote1, char_list, quote2)
and parse_char tokens = match tokens#pop with 
    | Lex.T_Char x -> Char (String.get x.value 0)
    | x -> raise (Expected_Something_Else("a character", x))
and parse_boolop tokens = match (tokens#pop) with
    | Lex.T_Equality _ -> Equal
    | Lex.T_Inequality _ -> Not_Equal
    | x -> raise (Expected_Something_Else("== | !=", x))
and parse_digit tokens = match (tokens#pop) with
    | Lex.T_Digit x -> Digit (int_of_string x.value)
    | x -> raise (Expected_Something_Else("a number", x))
and parse_intop tokens = match (tokens#pop) with
    | Lex.T_Plus _ -> Plus
    | x -> raise (Expected_Something_Else("+", x))
and parse_quote tokens = match (tokens#pop) with
    | Lex.T_Double_Quote _ -> Quote
    | x -> raise (Expected_Something_Else("\"", x))
and parse_char_list tokens = match (tokens#pop) with
    | Lex.T_String x -> Char_List (x.value)
    | x -> raise (Expected_Something_Else("a string", x))
and parse_dollar_sign tokens = match (tokens#pop) with
    | Lex.T_Dollar_Sign _ -> Dollar_Sign
    | x -> raise (Expected_Something_Else("dollar sign", x))
