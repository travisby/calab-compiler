type token_data = {lineno: int; charpos: int};;
type token = Error
   | T_OpenBrace of token_data
   | T_CloseBrace of token_data
   | T_print of token_data
   | T_OpenParen of token_data
   | T_CloseParen of token_data
   | T_while of token_data
   | T_id of token_data
   | T_if of token_data
   | T_int of token_data
   | T_string of token_data
   | T_boolean of token_data
   | T_char of token_data
   | T_digit of token_data
   | T_assignment of token_data
   | T_equality of token_data
   | T_inequality of token_data
   | T_false of token_data
   | T_true of token_data
   | T_plus of token_data
;;
