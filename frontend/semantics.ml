let log_trace = Log.log_trace_func "semantic_analyzer";;
let log_warn = Log.log_warn_func "semantic_analyzer";;
let log_error = Log.log_error_func "semantic_analyzer";;

let rec ast_of_cst cst = match cst with
    | Cst.Program (block, _) -> Ast.Program (ast_of_cst block)
    | Cst.Block (_, statement_list, _) ->
            let rec get_all_statements xs = match xs with
                | Cst.Empty_Statement_List -> []
                | Cst.Statement_List (x, xs) -> [ast_of_cst x] @ (get_all_statements xs)
                (* Pretty please don't use this function for anything else *)
                | _ -> raise Not_found
            in Ast.Block (get_all_statements statement_list)
    | Cst.Statement_Print_Statement statement
        | Cst.Statement_Assignment_Statement statement
        | Cst.Statement_Var_Decl statement
        | Cst.Statement_While_Statement statement
        | Cst.Statement_If_Statement statement
        | Cst.Statement_Block statement
    -> ast_of_cst statement
    | Cst.Print_Statement (_, expr, _) -> Ast.Print_Statement (ast_of_cst expr)
    | Cst.Assignment_Statement (id, _, value) -> Ast.Assignment_Statement (ast_of_cst id, ast_of_cst value)
    | Cst.Var_Decl (type_of, id) -> Ast.Var_Decl (ast_of_cst type_of, ast_of_cst id)
    | Cst.While_Statement (boolean_expr, block) -> Ast.While_Statement (ast_of_cst boolean_expr, ast_of_cst block)
    | Cst.If_Statement (boolean_expr, block) -> Ast.If_Statement (ast_of_cst boolean_expr, ast_of_cst block)
    | Cst.Expr_Int_Expr expr
        | Cst.Expr_String_Expr expr
        | Cst.Expr_Id_Expr expr
        | Cst.Expr_Boolean_Expr expr
    -> ast_of_cst expr
    (* Only plus exists in our language *)
    | Cst.Int_Expr (digit, intop, expr) when intop = Cst.Plus -> Ast.Addition (ast_of_cst digit, ast_of_cst expr)
    | Cst.String_Expr (_, char_list, _) ->
            let rec get_all_chars xs = match xs with
                | Cst.Empty_Char_List -> []
                | Cst.Char_List (x, xs) -> [ast_of_cst x] @ (get_all_chars xs)
                (* Pretty please don't use this function for anything else *)
                | _ -> raise Not_found
            in Ast.Char_List (get_all_chars char_list)
    | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Equal -> Ast.Equallity_Test (ast_of_cst expr1, ast_of_cst expr2)
    | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) when boolop = Cst.Not_Equal -> Ast.Inequallity_Test (ast_of_cst expr1, ast_of_cst expr2)
    | Cst.Id char_of_id -> Ast.Id char_of_id
    | Cst.Int -> Ast.Int
    | Cst.String -> Ast.String
    | Cst.Boolean -> Ast.Boolean
    | Cst.Char _char -> Ast.Char _char
    | Cst.Space -> Ast.Char ' '
    | Cst.Digit string_of_int -> Ast.Digit (int_of_string string_of_int)
    | _ -> raise Not_found
;;

let rec symboltable_of_cst ?(st=new Symbol_table.symboltable) tree  = match tree with
    | Cst.Program (x, _) ->
            symboltable_of_cst ~st x
    | Cst.Block (_, xs, _) ->
            log_trace "Entering new scope";
            st#enter;
            symboltable_of_cst ~st xs;
            log_trace "Exiting current scope";
            st#exit
    | Cst.Statement_List (x, xs) ->
            symboltable_of_cst ~st x;
            symboltable_of_cst ~st xs
    | Cst.Statement_Var_Decl x ->
            symboltable_of_cst ~st x
    | Cst.Statement_While_Statement x ->
            symboltable_of_cst ~st x
    | Cst.Statement_If_Statement x ->
            symboltable_of_cst ~st x
    | Cst.Statement_Block x ->
            symboltable_of_cst ~st x
    | Cst.Var_Decl (_type, name) ->
            log_trace "Adding var to the symbol table";
            st#add name _type
    | Cst.While_Statement (_, x) ->
            symboltable_of_cst ~st x
    | Cst.If_Statement (_, x) ->
            symboltable_of_cst ~st x
    | Cst.Statement_Assignment_Statement x ->
            symboltable_of_cst ~st x
    | Cst.Assignment_Statement (id, _, _val) ->
            log_trace "Setting var into symbol table";
            st#set id _val
    | _ -> ()

let analyze cst = ()
