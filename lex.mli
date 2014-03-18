exception UnrecognizedTokenError of string * int
exception CannotContainUnderscoreError
type token_data = { lineno : int; value : string; charno: int;}
type token =
    T_Open_Brace of token_data
  | T_Close_Brace of token_data
  | T_Print of token_data
  | T_Open_Paren of token_data
  | T_Close_Paren of token_data
  | T_While of token_data
  | T_Id of token_data
  | T_If of token_data
  | T_Int of token_data
  | T_String of token_data
  | T_Char_List of token_data
  | T_Boolean of token_data
  | T_Char of token_data
  | T_Digit of token_data
  | T_Assignment of token_data
  | T_Equality of token_data
  | T_Inequality of token_data
  | T_False of token_data
  | T_True of token_data
  | T_Plus of token_data
  | T_Double_Quote of token_data
  | T_Dollar_Sign of token_data
val lex : string -> token list
val token_as_string : token -> string
