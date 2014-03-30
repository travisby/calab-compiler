type ast =
    Program of ast
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
