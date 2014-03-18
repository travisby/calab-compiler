(** Our Parser *)

(** Exception to be raised when we got the wrong token.  The first parameter is
 * what we expected, the second is what we got *)
exception Expected_Something_Else of string * Lex.token
(** Exception to be raised when a symbol already exists in our table *)
exception Already_Exists_In_table
(** General error TODO Replace *)
exception CompilerError
(** An exception to be raised when we try to exit the global scope *)
exception CannotExitGlobalScope
(** Our Concrete Syntax Tree *)
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
  | Expr_Boolean_Expr of cst
  | Int_Expr of cst * cst * cst
  | String_Expr of cst * cst * cst
  | Boolean_Expr of cst * cst * cst * cst * cst
  | Id of string
  | Char_List of cst * cst
  | Empty_Char_List
  | Int
  | String
  | Boolean
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
  | Null (** USED INTERNALLY *)
(** The individual symboltable for a particular level of scope *)
class ['a, 'b] table :
  object
    method add : 'a -> 'b -> unit
    method get : 'a -> 'b
    method mem : 'a -> bool
    method set : 'a -> 'b -> unit
  end
(** The scope of our program for a symboltable *)
type scope =
    Global of (string, cst) table * scope array (** The Global scope for our
    program.  Holds the global symbol table, and an array of children scopes *)
    | Scope of (string, cst) table * scope array * scope ref (** The internal
    scope for our global scope.  Holds the local symbol table, an array of
    children scopes, and its parent reference *)
(** Our symbol table *)
class symboltable :
  object
    method add : cst -> cst -> unit (** add element to the current scope *)
    method enter : unit (** Enter a new scope *)
    method exit : unit (** Exit into the parent scope *)
    method set : cst -> cst -> unit
  end
val parse : Lex.token list -> cst * symboltable (** Create the Concrete Syntax
Tree, and Symbol Table from a list of tokens *)
