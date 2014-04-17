type obj = {pos:Utils.pos}
type operator = {pos:Utils.pos; str:string}
exception Type_Error of obj * operator * obj

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
                        let typeof = st#get_type_of id in
                        let ast_id =
                            begin
                                match id with
                                    | Cst.Id (x, pos) -> Ast.Id (x, pos)
                                    | _ -> raise Not_found
                            end
                        in
                        let value = inner_func value in
                        let assignment_statement = Assignment_Statement (inner_func id, value, pos) in
                        begin
                            match value, typeof with
                                | Addition _, Cst.Int _ -> assignment_statement
                                | Digit _, Cst.Int _ -> assignment_statement
                                | Char_List _, Cst.String _ -> assignment_statement
                                | Equallity_Test _, Cst.Boolean _ -> assignment_statement
                                | Inequallity_Test _, Cst.Boolean _ -> assignment_statement
                                | True _, Cst.Boolean _ -> assignment_statement
                                | False _ , Cst.Boolean _ -> assignment_statement
                                | Id (x, pos), _ when typeof = (st#get_type_of (Cst.Id (x, pos))) -> assignment_statement
                                | x, _ ->
                                    log_error ("Type Error.  Cannot assign  " ^ string_of_ast ast_id ^ " to " ^ string_of_ast x);
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
                                match expr1, expr2 with
                                    | Addition (_, _, _), Addition (_, _, _)
                                        | Addition (_, _, _), Digit (_, _)
                                        | Digit (_, _), Addition (_, _, _)
                                        | Digit (_, _), Digit (_, _)
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Char_List (_, _), Char_List (_, _) -> Equallity_Test (expr1, expr2, pos)
                                    | Equallity_Test (_, _, _), Equallity_Test (_, _, _)
                                        | Equallity_Test (_, _, _), Inequallity_Test (_, _, _)
                                        | Equallity_Test (_, _, _), True _
                                        | Equallity_Test (_, _, _), False _
                                        | Inequallity_Test (_, _, _), Equallity_Test (_, _, _)
                                        | Inequallity_Test (_, _, _), Inequallity_Test (_, _, _)
                                        | True _, Equallity_Test (_, _, _)
                                        | False _, Inequallity_Test (_, _, _)
                                        | True _, True _
                                        | True _, False _
                                        | False _, True _
                                        | False _, False _
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), Addition _  when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Int _ ->  true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), Digit _  when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Int _ -> true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), Char_List _  when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.String _ -> true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), True _ when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), False _ when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), Equallity_Test _ when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        ) 
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | Id (x, pos), Inequallity_Test _ when (
                                        match st#get_type_of (Cst.Id (x, pos)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        )
                                    -> Equallity_Test (expr1, expr2, pos)
                                    | x, y ->
                                            log_error ("Type Error.  Cannot compare  " ^ string_of_ast x ^ " and " ^ string_of_ast y);
                                            raise (Type_Error ({pos=pos}, {pos=pos; str="=="}, {pos=pos}))
                                end
                        | Cst.Not_Equal _ ->
                            begin
                                let expr1 = inner_func expr1 in
                                let expr2 = inner_func expr2 in
                                match expr1, expr2 with
                                    | Addition (_, _, pos), Addition (_, _, pos2)
                                        | Addition (_, _, pos), Digit (_, pos2)
                                        | Digit (_, pos), Addition (_, _, pos2)
                                        | Digit (_, pos), Digit (_, pos2)
                                    -> Inequallity_Test (expr1, expr2, pos)
                                    | Char_List (_, _), Char_List (_, _) -> Inequallity_Test (expr1, expr2, pos)
                                    | Equallity_Test (_, _, pos), Equallity_Test (_, _, pos2)
                                        | Equallity_Test (_, _, pos), Inequallity_Test (_, _, pos2)
                                        | Equallity_Test (_, _, pos), True pos2
                                        | Equallity_Test (_, _, pos), False pos2
                                        | Inequallity_Test (_, _, pos), Equallity_Test (_, _, pos2)
                                        | Inequallity_Test (_, _, pos), Inequallity_Test (_, _, pos2)
                                        | True pos, Equallity_Test (_, _, pos2)
                                        | False pos, Inequallity_Test (_, _, pos2)
                                        | True pos, True pos2
                                        | True pos, False pos2
                                        | False pos, True pos2
                                        | False pos, False pos2
                                    -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), Addition (_, _, pos2)  when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Int _  -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), Digit (_, pos2)  when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Int _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), Char_List (_, pos2)  when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.String _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), True pos2 when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), False pos2 when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), Equallity_Test (_, _, pos2) when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | Id (x, lineno), Inequallity_Test (_, _, pos2) when (
                                        match st#get_type_of (Cst.Id (x, lineno)) with
                                            | Cst.Boolean _ -> true
                                            | _ -> false
                                        ) -> Inequallity_Test (expr1, expr2, pos)
                                    | x, y ->
                                            log_error ("Type Error.  Cannot compare  " ^ string_of_ast x ^ " and " ^ string_of_ast y);
                                            raise (Type_Error ({pos=pos}, {pos=pos; str="!="}, {pos=pos}))
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
