type ast =
    Program of ast * Utils.pos
  | Block of ast list * Utils.pos
  | Print_Statement of ast * Utils.pos
  | Assignment_Statement of ast * ast * Utils.pos
  | Var_Decl of ast * ast * Utils.pos
  | While_Statement of ast * ast * Utils.pos
  | If_Statement of ast * ast * Utils.pos
  | Int of Utils.pos
  | Boolean of Utils.pos
  | String of Utils.pos
  | Id of char * Utils.pos
  | Plus of ast * ast * Utils.pos
  | Char_List of ast list * Utils.pos
  | Addition of ast * ast * Utils.pos
  | Equallity_Test of ast * ast * Utils.pos
  | Inequallity_Test of ast * ast * Utils.pos
  | Char of char * Utils.pos
  | Digit of int * Utils.pos
  | True of Utils.pos
  | False of Utils.pos
val string_of_ast : ?indent:string -> ast -> string
