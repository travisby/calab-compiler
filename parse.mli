exception Expected_Something_Else of string * Lex.token
exception Already_Exists_In_table
exception CompilerError
exception CannotExitGlobalScope
type cst =
    Program of cst * cst
  | Block of cst * cst * cst
  | Emtpy_Statement_List
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
  | Int_Expr of cst * cst * cst
  | String_Expr of cst * cst * cst
  | Boolean_Expr of cst * cst * cst * cst * cst
  | Id of cst
  | Char_List of string
  | Empty_Char_List
  | Int
  | String
  | Boolean
  | Char of char
  | Space
  | Digit of int
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
class ['a, 'b] table :
  object
    val mutable t : ('a, 'b) Hashtbl.t
    method add : 'a -> 'b -> unit
    method get : 'a -> 'b
    method private mem : 'a -> bool
  end
type scope =
    Global of (string, string) table * scope array
    | Scope of (string, string) table * scope array * scope ref
class symboltable :
  object
    val mutable current_scope : scope ref
    val mutable head : scope ref
    method add : cst -> cst -> unit
    method enter : unit
    method exit : unit
  end
val parse : Lex.token list -> cst * symboltable
