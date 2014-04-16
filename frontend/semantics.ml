exception Type_Error;;

open Ast;;

let log_trace = Log.log_trace_func "semantic_analyzer";;
let log_warn = Log.log_warn_func "semantic_analyzer";;
let log_error = Log.log_error_func "semantic_analyzer";;
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
            | Cst.Program (block, _) -> Ast.Program (inner_func block)
            | Cst.Block (_, statement_list, _) ->
                    st#enter;
                    let rec gathered_statements sl = 
                        begin
                            match sl with
                                | Cst.Statement_List (statement, statement_list) -> [statement] @ (gathered_statements statement_list)
                                | Cst.Empty_Statement_List -> []
                                | _ -> raise Not_found
                        end
                    in
                    let statements = gathered_statements statement_list in
                    st#exit;
                    Block (List.map inner_func statements)
            (* These should be handled in Block *)
            | Cst.Statement_List (_, _) -> raise Not_found
            | Cst.Empty_Statement_List -> raise Not_found
            | Cst.Statement_Print_Statement statement
            | Cst.Statement_Assignment_Statement statement
            | Cst.Statement_Var_Decl statement
            | Cst.Statement_While_Statement statement
            | Cst.Statement_If_Statement statement
            | Cst.Statement_Block statement -> inner_func statement
            | Cst.Print_Statement (_, expr, _) -> Print_Statement (inner_func expr)
            | Cst.Assignment_Statement (id, _, value) ->
                    begin
                        st#assign id;
                        let typeof = st#get_type_of id in
                        let ast_id = match id with
                            | Cst.Id x -> Ast.Id x
                            | _ -> raise Not_found
                        in
                        let value = inner_func value in
                        let assignment_statement = Assignment_Statement (inner_func id, value) in
                        match value with
                            | Addition _ when typeof = Cst.Int -> assignment_statement
                            | Digit _ when typeof = Cst.Int -> assignment_statement
                            | Char_List _ when typeof = Cst.String -> assignment_statement
                            | Equallity_Test _ when typeof = Cst.Boolean -> assignment_statement
                            | Inequallity_Test _ when typeof = Cst.Boolean -> assignment_statement
                            | True when typeof = Cst.Boolean -> assignment_statement
                            | False when typeof = Cst.Boolean -> assignment_statement
                            | Id x when typeof = (st#get_type_of (Cst.Id x)) -> assignment_statement
                            | x ->
                                    log_error ("Type Error.  Cannot assign  " ^ string_of_ast ast_id ^ " to " ^ string_of_ast x);
                                    raise Type_Error
                    end
            | Cst.Var_Decl (typeof, id) ->
                    st#add id typeof;
                    Var_Decl (inner_func typeof, inner_func id)
            | Cst.While_Statement (test, block) -> While_Statement (inner_func test, inner_func block)
            | Cst.If_Statement (test, block) -> If_Statement (inner_func test, inner_func block)
            | Cst.Expr_Int_Expr expr
            | Cst.Expr_String_Expr expr
            | Cst.Expr_Boolean_Expr expr -> inner_func expr
            | Cst.Expr_Id_Expr id ->
                    st#use id;
                    inner_func id
            | Cst.Int_Expr (digit, _, expr) ->
                    begin
                        let expr = inner_func expr in
                        let digit2 = inner_func digit in
                        match expr with
                            | Addition _
                                | Digit _
                            -> Addition (inner_func digit, expr)
                                | Id x when st#get_type_of (Cst.Id x) = Cst.Int -> Addition (inner_func digit, expr)
                            | _ ->
                                    log_error ("Type Error.  Cannot add  " ^ string_of_ast digit2 ^ " to " ^ string_of_ast expr);
                                    raise Type_Error
                    end
            | Cst.String_Expr (_, char_list, _) ->
                    let rec gathered_chars cl = 
                        begin
                            match cl with
                                | Cst.Char_List (character, char_list) -> character :: (gathered_chars char_list)
                                | Cst.Empty_Char_List -> []
                                | _ -> raise Not_found
                        end
                    in
                    let chars = gathered_chars char_list in
                    Char_List (List.map inner_func chars)
            | Cst.Char_List (_, _) -> raise Not_found
            | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Equal ->
                    begin
                        let expr1 = inner_func expr1 in
                        let expr2 = inner_func expr2 in
                        match expr1, expr2 with
                            | Addition _, Addition _
                                | Addition _, Digit _
                                | Digit _, Addition _
                                | Digit _, Digit _
                            -> Equallity_Test (expr1, expr2)
                            | Char_List _, Char_List _ -> Equallity_Test (expr1, expr2)
                            | Equallity_Test _, Equallity_Test _
                                | Equallity_Test _, Inequallity_Test _
                                | Equallity_Test _, True
                                | Equallity_Test _, False
                                | Inequallity_Test _, Equallity_Test _
                                | Inequallity_Test _, Inequallity_Test _
                                | True, Equallity_Test _
                                | False, Inequallity_Test _
                                | True, True
                                | True, False
                                | False , True
                                | False , False
                            -> Equallity_Test (expr1, expr2)
                            | Id x, Addition _  when (st#get_type_of (Cst.Id x)) = Cst.Int -> Equallity_Test (expr1, expr2)
                            | Id x, Digit _  when (st#get_type_of (Cst.Id x)) = Cst.Int -> Equallity_Test (expr1, expr2)
                            | Id x, Char_List _  when (st#get_type_of (Cst.Id x)) = Cst.String -> Equallity_Test (expr1, expr2)
                            | Id x, True when (st#get_type_of (Cst.Id x)) = Cst.Boolean -> Equallity_Test (expr1, expr2)
                            | Id x, False when (st#get_type_of (Cst.Id x)) = Cst.Boolean -> Equallity_Test (expr1, expr2)
                            | Id x, Equallity_Test _ when (st#get_type_of (Cst.Id x)) = Cst.Boolean -> Equallity_Test (expr1, expr2)
                            | Id x, Inequallity_Test _ when (st#get_type_of (Cst.Id x)) = Cst.Boolean -> Equallity_Test (expr1, expr2)
                            | x, y ->
                                    log_error ("Type Error.  Cannot compare  " ^ string_of_ast x ^ " and " ^ string_of_ast y);
                                    raise Type_Error
                        end
            | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Not_Equal ->
                    Inequallity_Test (inner_func expr1, inner_func expr2)
            | Cst.Id x -> Id x
            | Cst.Empty_Char_List
            | Cst.Int -> Int
            | Cst.String -> String
            | Cst.Boolean -> Boolean
            | Cst.Char x -> Char x
            | Cst.Space -> Char ' '
            | Cst.Digit d -> Digit (int_of_string d)
            | Cst.True -> True
            | Cst.False -> False
            | _ -> raise Not_found
    in inner_func cst
