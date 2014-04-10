open Lex;;
open Symbol_table;;
open Cst;;

exception Expected_Something_Else of string * token;;
exception CompilerError;;

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
        method empty = List.length q == 0
    end
;;

(* possible tokens... T_OpenBrace, T_CloseBrace, T_print, T_OpenParen,
 * T_CloseParen, T_while, T_id, T_if, T_int, T_string, T_boolean, T_char,
 * T_digit, T_assignment, T_equality, T_inequality, T_false, T_true, T_plus,
 * T_doublequote
 *)

let log_trace = Log.log_trace_func "parse";;
let log_warn = Log.log_warn_func "parse";;
let log_error = Log.log_error_func "parse";;

(*
 * non-terminals peek, terminals pop
 *)
let rec parse tokens =
    try
        let cst = parse_program (new queue tokens) in
        let symboltable = new symboltable in
        (* Make sure we do this before returning... *)
        let _ = symboltable_of_cst ~st:symboltable cst in
        (cst, symboltable)
    with x -> match x with
        | Expected_Something_Else (expected, actual) ->
                log_error ("Expected " ^ expected ^ " but got " ^ (token_as_string actual));
                raise x
        | Already_Exists_In_table ->
                log_error "Cannot redeclare variable";
                raise x
        | CompilerError ->
                log_error "Unknown compiler error";
                raise x 
        | CannotExitGlobalScope ->
                log_error "Cannot exit global scope in symbol table";
                raise x
        | IncorrectCSTElementsInSymbolTableError ->
                log_error "Incorrect elements were passed into the symbol table functions";
                raise x
        | Does_Not_Exist_In_Table ->
                log_error "Variable was not declared before assigned";
                raise x
        | _ -> raise x
and parse_program tokens =
    log_trace "Expecting Program";
    let block = parse_block tokens in
    (*
     * Because this is an expected, or "okay" case, we are better off using an
     * if statement.  This is not an exceptional case
     *)
    let dollar_sign =
        if
            not tokens#empty
        then
            parse_dollar_sign tokens
        (*
         * Begin allows us to perform multiple statements where only one is
         * expected
         *)
        else begin
            log_warn "Missing $.  Adding for you!";
            Dollar_Sign
        end
    in
    log_trace "Got Program!";
    if
        not tokens#empty
    then
        log_warn "Extra text after $.  Ignoring it";
    Program (block, dollar_sign)
 (*
  * because we join with "and" and a previously recursive function, this is also recursive
  * We use the let .. in syntax to force order of evaluation.  OCaml makes
    no guarantees over the order of execution of parameters
    http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora029.html 
  *)
and parse_block tokens =
    log_trace "Expecting Block";
    (* these are analogous to local variables *)
    let ob = parse_open_brace tokens in
    let statement_list = parse_statement_list tokens in
    let cb = parse_close_brace tokens in
    (* this is the return value *)
    log_trace "Got Block!";
    Block (ob, statement_list, cb)
and parse_open_brace tokens =
    log_trace "Expecting {";
    match tokens#pop with
    | T_Open_Brace _ ->
            log_trace "Got {!";
            Open_Brace
    | x -> raise (Expected_Something_Else("Open Brace", x))
and parse_close_brace tokens =
    log_trace "Expecting }";
    match tokens#pop with
    | T_Close_Brace _ ->
            log_trace "Got }!";
            Close_Brace
    | x -> raise (Expected_Something_Else("Close Brace", x))

(*
 * We must assume the parse_statement into this function, or else we wouldn't
 * know whether to issue an empty statement list, or an unexpected token error
 *)
and parse_statement_list tokens =
    log_trace "Expecting statement list (Print|Id|Type|While|If|Block and statement list, OR nothing";
    match tokens#peek with
    | T_Print x ->
            let print_statement = parse_print_statement tokens in
            let statement_list = parse_statement_list tokens in
            log_trace "Got statement list!";
            Statement_List (Statement_Print_Statement (print_statement), statement_list)
    | T_Id x ->
            let assignment_statement = parse_assignment_statement tokens in
            let statement_list = parse_statement_list tokens in
            log_trace "Got statement list!";
            Statement_List (Statement_Assignment_Statement (assignment_statement), statement_list)
    | T_Int x ->
            let var_declstatement = parse_var_decl_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Var_Decl (var_declstatement), statement_list)
    | T_String x ->
            let var_declstatement = parse_var_decl_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Var_Decl (var_declstatement), statement_list)
    | T_Boolean x ->
            let var_declstatement = parse_var_decl_statement tokens in
            let statement_list = parse_statement_list tokens in
            Statement_List (Statement_Var_Decl (var_declstatement), statement_list)
    | T_While x ->
            let while_statement = parse_while_statement tokens in
            let statement_list = parse_statement_list tokens in
            log_trace "Got statement list!";
            Statement_List (Statement_While_Statement (while_statement), statement_list)
    | T_If x ->
            let if_statement = parse_if_statement tokens in
            let statement_list = parse_statement_list tokens in
            log_trace "Got statement list!";
            Statement_List (Statement_If_Statement (if_statement), statement_list)
    | T_Open_Brace x ->
            let block = parse_block tokens in
            let statement_list = parse_statement_list tokens in
            log_trace "Got statement list!";
            Statement_List (Statement_Block (block), statement_list)
    | _ -> 
            log_trace "Got empty statement list!";
            Empty_Statement_List
and parse_print_statement tokens =
    log_trace "Expecting print";
    match tokens#pop with
    | T_Print _ ->
            let op = parse_open_paren tokens in
            let expr = parse_expr tokens in
            let cp = parse_close_paren tokens in
            log_trace "Got print!";
            Print_Statement (op, expr, cp)
    | x -> raise (Expected_Something_Else("Print", x))
and parse_assignment_statement tokens = 
    log_trace "Expecting assignment statement (id = val)";
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    let equals = parse_equals tokens in
    let expr = parse_expr tokens in
    log_trace "Got assignment statement!";
    Assignment_Statement (_id, equals, expr)
and parse_var_decl_statement tokens =
    log_trace "Expecting variable declaration (<type> <name>)";
    (* remember, type is a reserved word, and _type  is already used as a type! *)
    let the_type = parse_type tokens in
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    log_trace "Got variable declaration!";
    Var_Decl (the_type, _id)
and parse_while_statement tokens =
    log_trace "Expecting while Statement (while block)";
    match tokens#pop with
    | T_While _ ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            log_trace "Got while statement!";
            While_Statement (boolean_expr, block)
    | x -> raise (Expected_Something_Else("While", x))
and parse_if_statement tokens =
    log_trace "Expecting if statement (if block)";
    match tokens#pop with
    | T_If _ ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            log_trace "Got if statement!";
            If_Statement (boolean_expr, block)
    | x -> raise (Expected_Something_Else("If", x))
and parse_open_paren tokens =
    log_trace "Expecting (";
    match tokens#pop with
    | T_Open_Paren _ ->
            log_trace "Got (!";
            Open_Paren
    | x -> raise (Expected_Something_Else("Open Paren", x))
and parse_expr tokens =
    log_trace "Expecting expr (int expr | string expr | id | true | false)";
    match tokens#peek with
    | T_Digit _ ->
            log_trace "Got expr!";
            Expr_Int_Expr (parse_int_expr tokens)
    | T_Double_Quote _ ->
            log_trace "Got expr!";
            Expr_String_Expr (parse_string_expr tokens)
    | T_Id _ ->
            log_trace "Got expr!";
            Expr_Id_Expr (parse_id tokens)
    | T_True _ ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens)
    | T_False _ ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens)
    | T_Open_Paren _ ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens)
    | x -> raise (Expected_Something_Else("Digit | Quote | Id", x))
and parse_close_paren tokens =
    log_trace "Expecting )";
    match tokens#pop with
    | T_Close_Paren _ ->
            log_trace "Got )!";
            Close_Paren
    | x -> raise (Expected_Something_Else("Close Paren", x))
and parse_id tokens =
    log_trace "Expecting <id>";
    match tokens#pop with 
    | T_Id x ->
            log_trace "Got <id>!";
            Id (String.get x.value 0)
    | x -> raise (Expected_Something_Else("Id", x))
and parse_equals tokens =
    log_trace "Expecting =";
    match tokens#pop with
    | T_Assignment _ ->
            log_trace "Got =!";
            Equals
    | x -> raise (Expected_Something_Else("=", x))
and parse_type tokens =
    log_trace "Expecting type (int | boolean | string)";
    match tokens#pop with
    | T_Int _ ->
            log_trace "Got type (int)!";
            Int
    | T_Boolean _ ->
            log_trace "Got type (boolean)!";
            Boolean
    | T_String _ ->
            log_trace "Got type (string)!";
            String
    | x -> raise (Expected_Something_Else("int | boolean | string", x))
and parse_boolean_expr tokens = match tokens#peek with
    | T_Open_Paren _ ->
        log_trace "Expecting boolean expr ( ( expr boolop expr ) )";
        let op = parse_open_paren tokens in
        let expr1 = parse_expr tokens in
        let boolop = parse_boolop tokens in
        let expr2 = parse_expr tokens in
        let cp = parse_close_paren tokens in
        log_trace "Got boolean expr!";
        Boolean_Expr (op, expr1, boolop, expr2, cp)
    | T_True _ -> let _ = tokens#pop in True 
    | T_False _ -> let _ = tokens#pop in False
    | x -> raise (Expected_Something_Else("boolexpr, boolval", x))
and parse_int_expr tokens =
    log_trace "Expecting int expr (num intop expr)";
    let digit1 = parse_digit tokens in
        match tokens#peek with
        | T_Plus _ -> 
            let intop = parse_intop tokens in
            let expr = parse_expr tokens in
            log_trace "Got int expr!";
            Int_Expr (digit1, intop, expr)
        | _ -> digit1
and parse_string_expr tokens =
    log_trace "Expecting string expr (\"str\")";
    let quote1 = parse_quote tokens in
    let char_list = parse_char_list tokens in
    let quote2 = parse_quote tokens in
    log_trace "Got string expr!";
    String_Expr (quote1, char_list, quote2)
and parse_quote tokens = match tokens#pop with
    | T_Double_Quote _ -> Quote
    | x -> raise (Expected_Something_Else ("\"", x))
and parse_char tokens =
    log_trace "Expecting character";
    match tokens#pop with 
    | T_Char x ->
            log_trace "Got character!";
            Char (String.get x.value 0)
    | x -> raise (Expected_Something_Else("a character", x))
and parse_boolop tokens =
    log_trace "Expecting boolop (==, !=)";
    match (tokens#pop) with
    | T_Equality _ ->
            log_trace "Got boolop(==)!";
            Equal
    | T_Inequality _ ->
            log_trace "Got boolop(!=)!";
            Not_Equal
    | x -> raise (Expected_Something_Else("== | !=", x))
and parse_digit tokens =
    log_trace "Expecting a digit";
    match (tokens#pop) with
    | T_Digit x ->
            log_trace "Got a digit!";
            Digit (x.value)
    | x -> raise (Expected_Something_Else("a number", x))
and parse_intop tokens =
    log_trace "Expecting an intop (+)";
    match (tokens#pop) with
    | T_Plus _ ->
            log_trace "Got an intop (+)!";
            Plus
    | x -> raise (Expected_Something_Else("+", x))
and parse_char_list tokens =
    log_trace "Expecting character list";
    match (tokens#peek) with
        | T_Char x when x.value = " " ->
                let space = parse_space tokens in
                let char_list = parse_char_list tokens in
                Char_List (space, char_list)
        | T_Char x ->
                let _char = parse_char tokens in
                let char_list = parse_char_list tokens in
                Char_List (_char, char_list)
        | _ -> Empty_Char_List
and  parse_space tokens = match tokens#pop with
    | T_Char x when x.value = " " -> Space
    | x -> raise (Expected_Something_Else ("space character", x))
and parse_dollar_sign tokens =
    log_trace "Expecting $";
    match (tokens#pop) with
    | T_Dollar_Sign _ ->
            log_trace "Got $!";
            Dollar_Sign
    | x -> raise (Expected_Something_Else("dollar sign", x))
and symboltable_of_cst ?(st=new symboltable) tree  = match tree with
    | Program (x, _) ->
            symboltable_of_cst ~st x
    | Block (_, xs, _) ->
            log_trace "Entering new scope";
            st#enter;
            symboltable_of_cst ~st xs;
            log_trace "Exiting current scope";
            st#exit
    | Statement_List (x, xs) ->
            symboltable_of_cst ~st x;
            symboltable_of_cst ~st xs
    | Statement_Var_Decl x ->
            symboltable_of_cst ~st x
    | Statement_While_Statement x ->
            symboltable_of_cst ~st x
    | Statement_If_Statement x ->
            symboltable_of_cst ~st x
    | Statement_Block x ->
            symboltable_of_cst ~st x
    | Var_Decl (_type, name) ->
            log_trace "Adding var to the symbol table";
            st#add name _type
    | While_Statement (_, x) ->
            symboltable_of_cst ~st x
    | If_Statement (_, x) ->
            symboltable_of_cst ~st x
    | Statement_Assignment_Statement x ->
            symboltable_of_cst ~st x
    | Assignment_Statement (id, _, _val) ->
            log_trace "Setting var into symbol table";
            st#set id _val
    | _ -> ()
;;
