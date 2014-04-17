open Lex;;
open Cst;;
open Utils;;

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
        parse_program (new queue tokens)
    with x -> match x with
        | Expected_Something_Else (expected, actual) ->
                log_error ("Expected " ^ expected ^ " but got " ^ (token_as_string actual));
                raise x
        | CompilerError ->
                log_error "Unknown compiler error";
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
            Dollar_Sign {charno=(-1); lineno=(-1)}
        end
    in
    log_trace "Got Program!";
    if
        not tokens#empty
    then
        log_warn "Extra text after $.  Ignoring it";
    Program (block, dollar_sign, {lineno=0; charno=0})
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
    let pos = match ob with
        | Open_Brace pos -> pos
        | _ -> raise Not_found
    in
    let statement_list = parse_statement_list tokens in
    let cb = parse_close_brace tokens in
    (* this is the return value *)
    log_trace "Got Block!";
    Block (ob, statement_list, cb, pos)
and parse_open_brace tokens =
    log_trace "Expecting {";
    match tokens#pop with
        | T_Open_Brace t ->
                log_trace "Got {!";
                Open_Brace {lineno=t.lineno; charno=t.charno}
        | x -> raise (Expected_Something_Else("Open Brace", x))
and parse_close_brace tokens =
    log_trace "Expecting }";
    match tokens#pop with
        | T_Close_Brace t ->
                log_trace "Got }!";
                Close_Brace {lineno=t.lineno; charno=t.charno}
        | x -> raise (Expected_Something_Else("Close Brace", x))

(*
 * We must assume the parse_statement into this function, or else we wouldn't
 * know whether to issue an empty statement list, or an unexpected token error
 *)
and parse_statement_list tokens =
    log_trace "Expecting statement list (Print|Id|Type|While|If|Block and statement list, OR nothing";
    match tokens#peek with
        | T_Print t ->
                let print_statement = parse_print_statement tokens in
                let statement_list = parse_statement_list tokens in
                log_trace "Got statement list!";
                Statement_List (Statement_Print_Statement (print_statement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_Id t ->
                let assignment_statement = parse_assignment_statement tokens in
                let statement_list = parse_statement_list tokens in
                log_trace "Got statement list!";
                Statement_List (Statement_Assignment_Statement (assignment_statement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_Int t ->
                let var_declstatement = parse_var_decl_statement tokens in
                let statement_list = parse_statement_list tokens in
                Statement_List (Statement_Var_Decl (var_declstatement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_String t ->
                let var_declstatement = parse_var_decl_statement tokens in
                let statement_list = parse_statement_list tokens in
                Statement_List (Statement_Var_Decl (var_declstatement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_Boolean t ->
                let var_declstatement = parse_var_decl_statement tokens in
                let statement_list = parse_statement_list tokens in
                Statement_List (Statement_Var_Decl (var_declstatement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_While t ->
                let while_statement = parse_while_statement tokens in
                let statement_list = parse_statement_list tokens in
                log_trace "Got statement list!";
                Statement_List (Statement_While_Statement (while_statement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_If t ->
                let if_statement = parse_if_statement tokens in
                let statement_list = parse_statement_list tokens in
                log_trace "Got statement list!";
                Statement_List (Statement_If_Statement (if_statement, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | T_Open_Brace t ->
                let block = parse_block tokens in
                let statement_list = parse_statement_list tokens in
                log_trace "Got statement list!";
                Statement_List (Statement_Block (block, {lineno=t.lineno; charno=t.charno}), statement_list, {lineno=t.lineno; charno=t.charno})
        | _ -> 
                log_trace "Got empty statement list!";
                Empty_Statement_List {lineno=0; charno=0}
and parse_print_statement tokens =
    log_trace "Expecting print";
    match tokens#pop with
    | T_Print t ->
            let op = parse_open_paren tokens in
            let expr = parse_expr tokens in
            let cp = parse_close_paren tokens in
            log_trace "Got print!";
            Print_Statement (op, expr, cp, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("Print", x))
and parse_assignment_statement tokens = 
    log_trace "Expecting assignment statement (id = val)";
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    let equals = parse_equals tokens in
    let expr = parse_expr tokens in
    log_trace "Got assignment statement!";
    let pos = match equals with
        | Equals x -> x
        | _ -> raise Not_found
    in
    Assignment_Statement (_id, equals, expr, pos)
and parse_var_decl_statement tokens =
    log_trace "Expecting variable declaration (<type> <name>)";
    (* remember, type is a reserved word, and _type  is already used as a type! *)
    let the_type = parse_type tokens in
    (* remember, id is already used as a type! *)
    let _id = parse_id tokens in
    let pos = match _id with
        | Id (_, x) -> x
        | _ -> raise Not_found
    in
    log_trace "Got variable declaration!";
    Var_Decl (the_type, _id, pos)
and parse_while_statement tokens =
    log_trace "Expecting while Statement (while block)";
    match tokens#pop with
    | T_While t ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            log_trace "Got while statement!";
            While_Statement (boolean_expr, block, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("While", x))
and parse_if_statement tokens =
    log_trace "Expecting if statement (if block)";
    match tokens#pop with
    | T_If t ->
            let boolean_expr = parse_boolean_expr tokens in
            let block = parse_block tokens in
            log_trace "Got if statement!";
            If_Statement (boolean_expr, block, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("If", x))
and parse_open_paren tokens =
    log_trace "Expecting (";
    match tokens#pop with
    | T_Open_Paren t ->
            log_trace "Got (!";
            Open_Paren {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("Open Paren", x))
and parse_expr tokens =
    log_trace "Expecting expr (int expr | string expr | id | true | false)";
    match tokens#peek with
    | T_Digit t ->
            log_trace "Got expr!";
            Expr_Int_Expr (parse_int_expr tokens, {lineno=t.lineno; charno=t.charno})
    | T_Double_Quote t ->
            log_trace "Got expr!";
            Expr_String_Expr (parse_string_expr tokens, {lineno=t.lineno; charno=t.charno})
    | T_Id t ->
            log_trace "Got expr!";
            Expr_Id_Expr (parse_id tokens, {lineno=t.lineno; charno=t.charno})
    | T_True t ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens, {lineno=t.lineno; charno=t.charno})
    | T_False t ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens, {lineno=t.lineno; charno=t.charno})
    | T_Open_Paren t ->
            log_trace "Got expr!";
            Expr_Boolean_Expr (parse_boolean_expr tokens, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("Digit | Quote | Id", x))
and parse_close_paren tokens =
    log_trace "Expecting )";
    match tokens#pop with
    | T_Close_Paren t ->
            log_trace "Got )!";
            Close_Paren {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("Close Paren", x))
and parse_id tokens =
    log_trace "Expecting <id>";
    match tokens#pop with 
    | T_Id t ->
            log_trace "Got <id>!";
            Id (String.get t.value 0, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("Id", x))
and parse_equals tokens =
    log_trace "Expecting =";
    match tokens#pop with
    | T_Assignment t ->
            log_trace "Got =!";
            Equals {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("=", x))
and parse_type tokens =
    log_trace "Expecting type (int | boolean | string)";
    match tokens#pop with
    | T_Int t ->
            log_trace "Got type (int)!";
            Int {lineno=t.lineno; charno=t.charno}
    | T_Boolean t ->
            log_trace "Got type (boolean)!";
            Boolean {lineno=t.lineno; charno=t.charno}
    | T_String t ->
            log_trace "Got type (string)!";
            String {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("int | boolean | string", x))
and parse_boolean_expr tokens = match tokens#peek with
    | T_Open_Paren t ->
        log_trace "Expecting boolean expr ( ( expr boolop expr ) )";
        let op = parse_open_paren tokens in
        let expr1 = parse_expr tokens in
        let boolop = parse_boolop tokens in
        let expr2 = parse_expr tokens in
        let cp = parse_close_paren tokens in
        log_trace "Got boolean expr!";
        Boolean_Expr (op, expr1, boolop, expr2, cp, {lineno=t.lineno; charno=t.charno})
    | T_True t ->
            let _ = tokens#pop in
            True {lineno=t.lineno; charno=t.charno}
    | T_False t ->
            let _ = tokens#pop in
            False {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("boolexpr, boolval", x))
and parse_int_expr tokens =
    log_trace "Expecting int expr (num intop expr)";
    let digit1 = parse_digit tokens in
        match tokens#peek with
        | T_Plus t -> 
            let intop = parse_intop tokens in
            let expr = parse_expr tokens in
            log_trace "Got int expr!";
            Int_Expr (digit1, intop, expr, {lineno=t.lineno; charno=t.charno})
        | t -> digit1
and parse_string_expr tokens =
    log_trace "Expecting string expr (\"str\")";
    let quote1 = parse_quote tokens in
    let char_list = parse_char_list tokens in
    let quote2 = parse_quote tokens in
    log_trace "Got string expr!";
    let pos = match quote1 with
        | Quote t -> t
        | _ -> raise Not_found
    in
    String_Expr (quote1, char_list, quote2, pos)
and parse_quote tokens = match tokens#pop with
    | T_Double_Quote t -> Quote {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else ("\"", x))
and parse_char tokens =
    log_trace "Expecting character";
    match tokens#pop with 
    | T_Char t ->
            log_trace "Got character!";
            Char (String.get t.value 0, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("a character", x))
and parse_boolop tokens =
    log_trace "Expecting boolop (==, !=)";
    match (tokens#pop) with
    | T_Equality t ->
            log_trace "Got boolop(==)!";
            Equal {lineno=t.lineno; charno=t.charno}
    | T_Inequality t ->
            log_trace "Got boolop(!=)!";
            Not_Equal {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("== | !=", x))
and parse_digit tokens =
    log_trace "Expecting a digit";
    match (tokens#pop) with
    | T_Digit t ->
            log_trace "Got a digit!";
            Digit (t.value, {lineno=t.lineno; charno=t.charno})
    | x -> raise (Expected_Something_Else("a number", x))
and parse_intop tokens =
    log_trace "Expecting an intop (+)";
    match (tokens#pop) with
    | T_Plus t ->
            log_trace "Got an intop (+)!";
            Plus{lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("+", x))
and parse_char_list tokens =
    log_trace "Expecting character list";
    match (tokens#peek) with
        | T_Char t when t.value = " " ->
                let space = parse_space tokens in
                let char_list = parse_char_list tokens in
                Char_List (space, char_list, {lineno=t.lineno; charno=t.charno})
        | T_Char t ->
                let _char = parse_char tokens in
                let char_list = parse_char_list tokens in
                Char_List (_char, char_list, {lineno=t.lineno; charno=t.charno})
        | _ -> Empty_Char_List {lineno=0; charno=0}
and  parse_space tokens = match tokens#pop with
    | T_Char t when t.value = " " -> Space {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else ("space character", x))
and parse_dollar_sign tokens =
    log_trace "Expecting $";
    match (tokens#pop) with
    | T_Dollar_Sign t ->
            log_trace "Got $!";
            Dollar_Sign {lineno=t.lineno; charno=t.charno}
    | x -> raise (Expected_Something_Else("dollar sign", x))
