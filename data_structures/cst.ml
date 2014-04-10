type cst =
    | Program of cst * cst
    | Block of cst * cst * cst
    | Empty_Statement_List
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
    | Expr_Boolean_Expr of cst
    | Int_Expr of cst * cst * cst
    | String_Expr of cst * cst * cst
    | Boolean_Expr of cst * cst * cst * cst * cst
    | Id of char
    | Char_List of cst * cst
    | Empty_Char_List
    | Int | String | Boolean
    | Char of char
    | Space
    | Digit of string
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
    | Null (* USED INTERNALLY *)
;;
