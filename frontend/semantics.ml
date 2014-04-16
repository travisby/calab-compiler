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
                    st#assign id;
                    Assignment_Statement (inner_func id, inner_func value)
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
            | Cst.Int_Expr (digit, _, expr) -> Addition (inner_func digit, inner_func expr)
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
            | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Equal -> Equallity_Test (inner_func expr1, inner_func expr2)
            | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Not_Equal -> Inequallity_Test (inner_func expr1, inner_func expr2)
            | Cst.Id x -> Id x
            | Cst.Empty_Char_List
            | Cst.Int -> Int
            | Cst.String -> String
            | Cst.Boolean -> Boolean
            | Cst.Char x -> Char x
            | Cst.Space -> Char ' '
            | Cst.Digit d -> Digit (int_of_string d)
            | _ -> raise Not_found
    in inner_func cst
