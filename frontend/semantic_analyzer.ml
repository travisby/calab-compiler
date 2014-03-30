let rec ast_of_cst cst = match cst with
    | Cst.Program (block, _) -> raise Not_found
    | Cst.Block (_, statement_list, _) -> raise Not_found
    | Cst.Emtpy_Statement_List -> raise Not_found
    | Cst.Statement_List (statement, statement_list) -> raise Not_found
    | Cst.Statement_Print_Statement statement -> raise Not_found
    | Cst.Statement_Assignment_Statement statement -> raise Not_found
    | Cst.Statement_Var_Decl statement -> raise Not_found
    | Cst.Statement_While_Statement statement -> raise Not_found
    | Cst.Statement_If_Statement statement -> raise Not_found
    | Cst.Statement_Block statement -> raise Not_found
    | Cst.Print_Statement (_, expr, _) -> raise Not_found
    | Cst.Assignment_Statement (id, _, value) -> raise Not_found
    | Cst.Var_Decl (type_of, id) -> raise Not_found
    | Cst.While_Statement (boolean_expr, block) -> raise Not_found
    | Cst.If_Statement (boolean_expr, block) -> raise Not_found
    | Cst.Expr_Int_Expr int_expr -> raise Not_found
    | Cst.Expr_String_Expr string_expr -> raise Not_found
    | Cst.Expr_Id_Expr id_expr -> raise Not_found
    | Cst.Expr_Boolean_Expr boolean_expr -> raise Not_found
    | Cst.Int_Expr (digit, intop, expr) -> raise Not_found
    | Cst.String_Expr (_, char_list, _) -> raise Not_found
    | Cst.Boolean_Expr (_, expr1, boolop, expr2, _) -> raise Not_found
    | Cst.Id char_of_id -> raise Not_found
    | Cst.Char_List (_char, char_list) -> raise Not_found
    | Cst.Empty_Char_List -> raise Not_found
    | Cst.Int -> raise Not_found
    | Cst.String -> raise Not_found
    | Cst.Boolean -> raise Not_found
    | Cst.Char _char -> raise Not_found
    | Cst.Space -> raise Not_found
    | Cst.Digit string_of_int -> raise Not_found
    | Cst.Equal -> raise Not_found
    | Cst.Not_Equal -> raise Not_found
    | Cst.False -> raise Not_found
    | Cst.True -> raise Not_found
    | Cst.Plus -> raise Not_found
    | Cst.Dollar_Sign -> raise Not_found
    | Cst.Open_Brace -> raise Not_found
    | Cst.Close_Brace -> raise Not_found
    | Cst.Open_Paren -> raise Not_found
    | Cst.Close_Paren -> raise Not_found
    | Cst.Equals -> raise Not_found
    | Cst.While -> raise Not_found
    | Cst.If -> raise Not_found
    | Cst.Quote -> raise Not_found
    | Cst.Null -> raise Not_found (* USED INTERNALLY *)
;;
