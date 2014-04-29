type obj = {pos:Utils.pos}
type operator = {pos:Utils.pos; str:string}
exception Type_Error of obj * operator * obj

open Ast;;
open Utils;;

let log_trace = Log.log_trace_func "semantic_analyzer";;
let log_warn = Log.log_warn_func "semantic_analyzer";;
let log_error = Log.log_error_func "semantic_analyzer";;
let typeof x st =
    (* this is zerod so we can compare type == type *)
    let pos = {charno=(-1); lineno=(-1)} in
    match x with
    | Id (x, _) ->
            begin
                match st#get_type_of (Cst.Id (x, pos)) with
                    | Cst.Int _ -> Int pos
                    | Cst.Boolean _ -> Boolean pos
                    | Cst.String _ -> String pos
                    | _ -> raise Not_found
            end
    | Addition _
        | Digit _
    -> Int pos
    | Equallity_Test _
        | Inequallity_Test _
        | True _
        | False _
    -> Boolean pos
    | Char_List (xs, _) -> String pos
    | _ -> raise Not_found
let analyze cst =
    (* This function adds to the ST AND type checks at the same time... this way
     * we use the correct scope for everything *)
    let st = new Symbol_table.symboltable in
    let rec inner_func cst =
        match cst with
            (*
             * We handle all of the statements and statement lists in block
             * because it allows us to care for empty statement lists and throw
             * them out.  Can't throw them out if we're returning the AST in our
             * inner_func!
             *)
            | Cst.Program (block, _, pos) -> Ast.Program (inner_func block, pos)
            | Cst.Block (_, statement_list, _, pos) ->
                    st#enter;
                    let rec gathered_statements sl = 
                        begin
                            match sl with
                                | Cst.Statement_List (statement, statement_list, _) -> [statement] @ (gathered_statements statement_list)
                                | Cst.Empty_Statement_List _ -> []
                                | _ -> raise Not_found
                        end
                    in
                    let statements = gathered_statements statement_list in
                    (*
                     * Word of warning!
                     * ----------------
                     *
                     * statements_as_ast will force execution of adding stuff
                     * to the symbol table.  If this isn't before
                     * st#warn_on_unused... then the Symbol Table will always be
                     * empty, because nothing has filled it yet!
                     *)
                    let statements_as_ast = List.map inner_func statements in
                    st#warn_on_unused_in_current_scope;
                    st#exit;
                    Block (statements_as_ast, pos)
            (* These should be handled in Block *)
            | Cst.Statement_List (_, _, _) -> raise Not_found
            | Cst.Empty_Statement_List _ -> raise Not_found
            | Cst.Statement_Print_Statement (statement, _)
            | Cst.Statement_Assignment_Statement (statement, _)
            | Cst.Statement_Var_Decl (statement, _)
            | Cst.Statement_While_Statement (statement, _)
            | Cst.Statement_If_Statement (statement, _)
            | Cst.Statement_Block (statement, _) -> inner_func statement
            | Cst.Print_Statement (_, expr, _, pos) -> Print_Statement (inner_func expr, pos)
            | Cst.Assignment_Statement (id, _, value, pos) ->
                    begin
                        st#assign id;
                        let id =
                            begin
                                match id with
                                    | Cst.Id (x, pos) -> Ast.Id (x, pos)
                                    | _ -> raise Not_found
                            end
                        in
                        let value = inner_func value in
                        if
                            (typeof id st) = (typeof value st)
                        then
                            Assignment_Statement (id, value, pos)
                        else 
                            begin
                                    log_error ("Type Error.  Cannot assign  " ^ string_of_ast id ^ " to " ^ string_of_ast value);
                                    log_error ("Type of id: " ^ (string_of_ast (typeof id st)));
                                    log_error ("Type of value: " ^ (string_of_ast (typeof value st)));
                                    raise (Type_Error ({pos=pos}, {pos=pos; str="="}, {pos=pos}))
                            end
                    end
            | Cst.Var_Decl (typeof, id, pos) ->
                    st#add id typeof;
                    Var_Decl (inner_func typeof, inner_func id, pos)
            | Cst.While_Statement (test, block, pos) -> While_Statement (inner_func test, inner_func block, pos)
            | Cst.If_Statement (test, block, pos) -> If_Statement (inner_func test, inner_func block, pos)
            | Cst.Expr_Int_Expr (expr, _)
            | Cst.Expr_String_Expr (expr, _)
            | Cst.Expr_Boolean_Expr (expr, _) -> inner_func expr
            | Cst.Expr_Id_Expr (id, _) ->
                    st#use id;
                    inner_func id
            | Cst.Int_Expr (digit, _, expr, pos) ->
                    begin
                        let expr = inner_func expr in
                        let digit2 = inner_func digit in
                        match expr with
                            | Addition _
                                | Digit _
                            -> Addition (inner_func digit, expr, pos)
                            | Id (x, pos) when (
                                match st#get_type_of (Cst.Id (x, pos)) with
                                    | Cst.Int _ -> true
                                    | _ -> false
                                ) ->
                                Addition (inner_func digit, expr, pos)
                            | _ ->
                                    log_error ("Type Error.  Cannot add  " ^ string_of_ast digit2 ^ " to " ^ string_of_ast expr);
                                    raise (Type_Error ({pos=pos}, {pos=pos; str="+"}, {pos=pos}))
                    end
            | Cst.String_Expr (_, char_list, _, pos) ->
                    let rec gathered_chars cl = 
                        begin
                            match cl with
                                | Cst.Char_List (character, char_list, _) -> character :: (gathered_chars char_list)
                                | Cst.Empty_Char_List _ -> []
                                | _ -> raise Not_found
                        end
                    in
                    let chars = gathered_chars char_list in
                    Char_List (List.map inner_func chars, pos)
            | Cst.Char_List (_, _, _) -> raise Not_found
            | Cst.Boolean_Expr (_, expr1, boolop, expr2, _, pos) ->
                    begin
                        match boolop with
                            | Cst.Equal _ ->
                                begin
                                    let expr1 = inner_func expr1 in
                                    let expr2 = inner_func expr2 in
                                    if
                                        (typeof expr1 st) = (typeof expr2 st)
                                    then
                                        Equallity_Test (expr1, expr2, pos)
                                    else 
                                        begin
                                                log_error ("Type Error.  Cannot compare  " ^ string_of_ast expr1 ^ " and " ^ string_of_ast expr2);
                                                log_error ("Type of expr1: " ^ (string_of_ast (typeof expr1 st)));
                                                log_error ("Type of expr2: " ^ (string_of_ast (typeof expr2 st)));
                                                raise (Type_Error ({pos=pos}, {pos=pos; str="=="}, {pos=pos}))
                                        end
                                    end
                            | Cst.Not_Equal _ ->
                                begin
                                    let expr1 = inner_func expr1 in
                                    let expr2 = inner_func expr2 in
                                    if
                                        (typeof expr1 st) = (typeof expr2 st)
                                    then
                                        Inequallity_Test (expr1, expr2, pos)
                                    else 
                                        begin
                                                log_error ("Type Error.  Cannot compare  " ^ string_of_ast expr1 ^ " and " ^ string_of_ast expr2);
                                                log_error ("Type of expr1: " ^ (string_of_ast (typeof expr1 st)));
                                                log_error ("Type of expr2: " ^ (string_of_ast (typeof expr2 st)));
                                                raise (Type_Error ({pos=pos}, {pos=pos; str="!="}, {pos=pos}))
                                        end
                                end
                            | _ -> raise Not_found
                    end
            | Cst.Id (x, pos) -> Id (x, pos)
            | Cst.Int pos -> Int pos
            | Cst.String pos -> String pos
            | Cst.Boolean pos -> Boolean pos
            | Cst.Char (x, pos) -> Char (x, pos)
            | Cst.Space pos -> Char (' ', pos)
            | Cst.Digit (d, pos) -> Digit (int_of_string d, pos)
            | Cst.True pos -> True pos
            | Cst.False pos -> False pos
            | _ -> raise Not_found
    in inner_func cst
