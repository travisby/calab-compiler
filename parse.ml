open Lex;;

exception Expected_Something_Else of string * token;;
exception Already_Exists_In_table;;
exception CompilerError;;
exception CannotExitGlobalScope;;

type cst =
    | Program of cst * cst
    | Block of cst * cst * cst
    | Emtpy_Statement_List
    | Statement_List of cst * cst
    | Statement_Print_Statement of cst
    | Statement_Assignment_Statement of cst
    | Statement_Var_Decl of cst
    | Statement_While_Statement of cst
    | Statement_If_Statement of cst
    | Statement_Block of cst
    | Print_Statement of cst * cst * cst
    | Assignment_Statement of cst * cst * cst
    | Var_Decl of cst * cst
    | While_Statement of cst * cst
    | If_Statement of cst * cst
    | Expr_Int_Expr of cst
    | Expr_String_Expr of cst
    | Expr_Id_Expr of cst
    | Int_Expr of cst * cst * cst
    | String_Expr of cst
    | Boolean_Expr of cst * cst * cst * cst * cst
    | Id of string
    | Char_List of string
    | Empty_Char_List
    | Int | String | Boolean
    | Char of char
    | Space
    | Digit of int
    | Equal
    | Not_Equal
    | False
    | True
    | Plus
    | Dollar_Sign
    | Open_Brace
    | Close_Brace
    | Open_Paren
    | Close_Paren
    | Equals
    | While
    | If
    | Quote
;;

class ['a, 'b] table =
    object (self)
        val mutable t = Hashtbl.create 0
        method private mem x = Hashtbl.mem t x
        method get x = Hashtbl.find t x
        method add (x:'a) (y:'b) =
            if
                self#mem x
            then
                raise Already_Exists_In_table
            else
                Hashtbl.add t x y
    end
;;

(*
 * We use an array because it is a mutable data structure.
 * We will be appending to the array to "add" new scopes
 *)
type scope =
    | Global of (string, string) table * scope array
    (* The last parameter is the parent scope *)
    | Scope of (string, string) table * scope array * scope ref
;;

class symboltable =
    (* A function that takes the unit () as its only parameter *)
    let scope_constructor parent = Scope (new table, [||], parent) in
    let scope_init = Global (new table, [||]) in
    object
        val mutable head = ref scope_init
        val mutable current_scope = ref scope_init
        method enter =
            (*
             * We seem (because we actually aren't) to be abusing let..in
             * statements, but it enforces execution order for us, which is not
             * guaranteed by the Ocaml language.  For all of the stuff depending
             * on state (say... moving the scope pointer!) it's important to
             * order the execution of statements
             *)
            let new_scope = ref (scope_constructor current_scope) in
            let scope_array = match !current_scope with
                | Scope (_, xs, _) -> xs
                | Global (_, xs) -> xs
            in
            (* Add the new scope! *)
            let _ = Array.append scope_array [|!new_scope|] in
            let _ = current_scope <- new_scope in
            ()
        method exit =
            let parent_scope = match !current_scope with
                | Scope (_, _, x) -> x
                | Global (_, _) -> raise CannotExitGlobalScope
            in
            let _ = current_scope <- parent_scope in
            ()
        method add (_:cst) (_:cst) = ()
    end
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
            Emtpy_Statement_List
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
    log_trace "Expecting expr (int expr | string expr | id)";
    match tokens#peek with
    | T_Digit _ ->
            log_trace "Got expr!";
            Expr_Int_Expr (parse_int_expr tokens)
    | T_Char_List _ ->
            log_trace "Got expr!";
            Expr_String_Expr (parse_string_expr tokens)
    | T_Id _ ->
            log_trace "Got expr!";
            Expr_Id_Expr (parse_id tokens)
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
            Id (x.value)
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
and parse_boolean_expr tokens =
    log_trace "Expecting boolean expr ( ( expr boolop expr ) )";
    let op = parse_open_paren tokens in
    let expr1 = parse_expr tokens in
    let boolop = parse_boolop tokens in
    let expr2 = parse_expr tokens in
    let cp = parse_close_paren tokens in
    log_trace "Got boolean expr!";
    Boolean_Expr (op, expr1, boolop, expr2, cp)
and parse_int_expr tokens =
    log_trace "Expecting int expr (num intop num)";
    let digit1 = parse_digit tokens in
    let intop = parse_intop tokens in
    let digit2 = parse_digit tokens in
    log_trace "Got int expr!";
    Int_Expr (digit1, intop, digit2)
and parse_string_expr tokens =
    log_trace "Expecting string expr (\"str\")";
    let char_list = parse_char_list tokens in
    log_trace "Got string expr!";
    String_Expr char_list
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
            Digit (int_of_string x.value)
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
    match (tokens#pop) with
    | T_Char_List x ->
        log_trace "Got character list!";
        Char_List x.value
    | x -> raise (Expected_Something_Else("a string", x))
and parse_dollar_sign tokens =
    log_trace "Expecting $";
    match (tokens#pop) with
    | T_Dollar_Sign _ ->
            log_trace "Got $!";
            Dollar_Sign
    | x -> raise (Expected_Something_Else("dollar sign", x))
and symboltable_of_cst ?(st=new symboltable) tree  = match tree with
    | Program (x, _) ->
            let _ = symboltable_of_cst ~st x in
            ()
    | Block (_, x, _) ->
            let _ = st#enter in
            let _ = symboltable_of_cst ~st x in
            let _ = st#exit in
            ()
    | Statement_List (x, y) ->
            (*
             * We don't care about the order of execution because we are
             * modifying an object, not worrying about return values
             *)
            let _ = symboltable_of_cst ~st x in
            let _ = symboltable_of_cst ~st y in
            ()
    | Statement_Var_Decl x ->
            let _ = symboltable_of_cst ~st x in
            ()
    | Statement_While_Statement x ->
            let _ = symboltable_of_cst ~st x in
            ()
    | Statement_If_Statement x ->
            let _ = symboltable_of_cst ~st x in
            ()
    | Statement_Block x ->
            let _ = symboltable_of_cst ~st x in
            ()
    | Var_Decl (_type, name) ->
            let _ = st#add name _type in
            ()
    | While_Statement (_, x) ->
            let _ = symboltable_of_cst ~st x in
            ()
    | If_Statement (_, x) ->
            let _ = symboltable_of_cst ~st x in
            ()
    | _ -> ()
;;
