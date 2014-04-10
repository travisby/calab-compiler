(** Our Parser *)

(** Exception to be raised when we got the wrong token.  The first parameter is
 * what we expected, the second is what we got *)
exception Expected_Something_Else of string * Lex.token
(** General error TODO Replace *)
exception CompilerError
(** Our Concrete Syntax Tree *)
val parse : Lex.token list -> Cst.cst (** Create the Concrete Syntax Tree, and Symbol Table from a list of tokens *)
