type ast =
    | Program of ast
    | Block of ast list
    | Print_Statement of ast
    | Assignment_Statement of ast * ast
    | Var_Decl of ast * ast
    | While_Statement of ast * ast
    | If_Statement of ast * ast
    | Int
    | Boolean
    | String
    | Id of char
    | Plus of ast * ast
    | Char_List of ast list
    | Addition of ast * ast
    | Equallity_Test of ast * ast
    | Inequallity_Test of ast * ast
    | Char of char
    | Digit of int
;;
