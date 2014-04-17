type cst =
    Program of cst * cst * Utils.pos
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
  | Null (** USED INTERNALLY *)

val string_of_cst : ?indent:string -> cst -> string
