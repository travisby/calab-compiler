type cst =
    | Program of cst * cst * Utils.pos
    | Block of cst * cst * cst * Utils.pos
    | Empty_Statement_List of Utils.pos
    | Statement_List of cst * cst * Utils.pos
    | Statement_Print_Statement of cst * Utils.pos
    | Statement_Assignment_Statement of cst * Utils.pos
    | Statement_Var_Decl of cst * Utils.pos
    | Statement_While_Statement of cst * Utils.pos
    | Statement_If_Statement of cst * Utils.pos
    | Statement_Block of cst * Utils.pos
    | Print_Statement of cst * cst * cst * Utils.pos
    | Assignment_Statement of cst * cst * cst * Utils.pos
    | Var_Decl of cst * cst * Utils.pos
    | While_Statement of cst * cst * Utils.pos
    | If_Statement of cst * cst * Utils.pos
    | Expr_Int_Expr of cst * Utils.pos
    | Expr_String_Expr of cst * Utils.pos
    | Expr_Id_Expr of cst * Utils.pos
    | Expr_Boolean_Expr of cst * Utils.pos
    | Int_Expr of cst * cst * cst * Utils.pos
    | String_Expr of cst * cst * cst * Utils.pos
    | Boolean_Expr of cst * cst * cst * cst * cst * Utils.pos
    | Id of char * Utils.pos
    | Char_List of cst * cst * Utils.pos
    | Empty_Char_List of Utils.pos
    | Int of Utils.pos
    | String of Utils.pos
    | Boolean of Utils.pos
    | Char of char * Utils.pos
    | Space of Utils.pos
    | Digit of string * Utils.pos
    | Equal of Utils.pos
    | Not_Equal of Utils.pos
    | False of Utils.pos
    | True of Utils.pos
    | Plus of Utils.pos
    | Dollar_Sign of Utils.pos
    | Open_Brace of Utils.pos
    | Close_Brace of Utils.pos
    | Open_Paren of Utils.pos
    | Close_Paren of Utils.pos
    | Equals of Utils.pos
    | While of Utils.pos
    | If of Utils.pos
    | Quote of Utils.pos
    | Null (* USED INTERNALLY *)
;;

let rec string_of_cst ?(indent="") cst =
    let tab = "    " in
    let prefix =
        "\n"
        ^ indent
    in
    match cst with
    | Program (block, ds, _) ->
            prefix
            ^ "Program"
            ^ string_of_cst ~indent:(indent ^ tab) block
            ^ string_of_cst ~indent:(indent ^ tab) ds
    | Block (ob, sl, cb, _) ->
            prefix
            ^ "Block"
            ^ string_of_cst ~indent:(indent ^ tab) ob
            ^ string_of_cst ~indent:(indent ^ tab) sl
            ^ string_of_cst ~indent:(indent ^ tab) cb
    | Empty_Statement_List _ ->
            prefix
            ^ "Empty Statement List"
    | Statement_List (s, sl, _) ->
            prefix
            ^ string_of_cst ~indent:(indent ^ tab) s
            ^ string_of_cst ~indent:(indent ^ tab) sl
    | Statement_Print_Statement (s, _) ->
            prefix
            ^ "Statement Print Statement"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Statement_Assignment_Statement (s, _) ->
            prefix
            ^ "Statement Assignment Statement"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Statement_Var_Decl (s, _) ->
            prefix
            ^ "Statement Var Decl"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Statement_While_Statement (s, _) ->
            prefix
            ^ "Statement While Statement"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Statement_If_Statement (s, _) ->
            prefix
            ^ "Statement If Statement"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Statement_Block (s, _) ->
            prefix
            ^ "Statement Block"
            ^ string_of_cst ~indent:(indent ^ tab) s
    | Print_Statement (ob, expr, cb, _) ->
            prefix
            ^ "Print Statement"
            ^ string_of_cst ~indent:(indent ^ tab) ob
            ^ string_of_cst ~indent:(indent ^ tab) expr
            ^ string_of_cst ~indent:(indent ^ tab) cb
    | Assignment_Statement (id, equals, expr, _) ->
            prefix
            ^ "Assignment Statement"
            ^ string_of_cst ~indent:(indent ^ tab) id
            ^ string_of_cst ~indent:(indent ^ tab) equals
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | Var_Decl (typeof, id, _) ->
            prefix
            ^ "Var Decl"
            ^ string_of_cst ~indent:(indent ^ tab) typeof
            ^ string_of_cst ~indent:(indent ^ tab) id
    | While_Statement (test, block, _) ->
            prefix
            ^ "While Statement"
            ^ string_of_cst ~indent:(indent ^ tab) test
            ^ string_of_cst ~indent:(indent ^ tab) block
    | If_Statement (test, block, _) ->
            prefix
            ^ "If Statement"
            ^ string_of_cst ~indent:(indent ^ tab) test
            ^ string_of_cst ~indent:(indent ^ tab) block
    | Expr_Int_Expr (expr, _) ->
            prefix
            ^ "Expr Int Expr"
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | Expr_String_Expr (expr, _) ->
            prefix
            ^ "Expr String Expr"
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | Expr_Id_Expr (expr, _) ->
            prefix
            ^ "Expr Id Expr"
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | Expr_Boolean_Expr (expr, _) ->
            prefix
            ^ "Expr Boolean Expr"
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | Int_Expr (digit, intop, expr, _) ->
            prefix
            ^ "Int Expr"
            ^ string_of_cst ~indent:(indent ^ tab) digit
            ^ string_of_cst ~indent:(indent ^ tab) intop
            ^ string_of_cst ~indent:(indent ^ tab) expr
    | String_Expr (quote1, char_list, quote2, _) ->
            prefix
            ^ "String Expr"
            ^ string_of_cst ~indent:(indent ^ tab) quote1
            ^ string_of_cst ~indent:(indent ^ tab) char_list
            ^ string_of_cst ~indent:(indent ^ tab) quote2
    | Boolean_Expr (op, expr1, boolop, expr2, cp, _) ->
            prefix
            ^ "Boolean Expr"
            ^ string_of_cst ~indent:(indent ^ tab) op
            ^ string_of_cst ~indent:(indent ^ tab) expr1
            ^ string_of_cst ~indent:(indent ^ tab) boolop
            ^ string_of_cst ~indent:(indent ^ tab) expr2
            ^ string_of_cst ~indent:(indent ^ tab) cp
    | Id (c, _) -> 
            prefix
            ^ "Id"
            ^ prefix ^ tab
            ^ Char.escaped c
    | Char_List (c, cl, _) ->
            prefix
            ^ "Char List"
            ^ string_of_cst ~indent:(indent ^ tab) c
            ^ string_of_cst ~indent:(indent ^ tab) cl
    | Empty_Char_List _ ->
            prefix
            ^ "Empty Char List"
    | Int _ ->
            prefix
            ^ "int"
    | String _ ->
            prefix
            ^ "string"
    | Boolean _ ->
            prefix
            ^ "boolean"
    | Char (c, _) ->
            prefix
            ^ (Char.escaped c)
    | Space _ ->
            prefix
            ^ "whitespace character"
    | Digit (d, _) ->
            prefix
            ^ d
    | Equal _ ->
            prefix
            ^ "="
    | Not_Equal _ ->
            prefix
            ^ "!="
    | False _ ->
            prefix
            ^ "false"
    | True _ ->
            prefix
            ^ "true"
    | Plus _ ->
            prefix
            ^ "+"
    | Dollar_Sign _ ->
            prefix
            ^ "$"
    | Open_Brace _ ->
            prefix
            ^ "{"
    | Close_Brace _ ->
            prefix
            ^ "}"
    | Open_Paren _ ->
            prefix
            ^ "("
    | Close_Paren _ ->
            prefix
            ^ ")"
    | Equals _ ->
            prefix
            ^ "=="
    | While _ ->
            prefix
            ^ "while"
    | If _ ->
            prefix
            ^ "if"
    | Quote _ ->
            prefix
            ^ "\""
    | _ -> raise Not_found
;;
