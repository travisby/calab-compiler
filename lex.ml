exception UnrecognizedTokenError of string * int;;

let id_regex = Str.regexp "[a-zA-Z][a-zA-z0-9_]*";;
let char_regex = Str.regexp "[a-zA-Z]";;
let digit_regex = Str.regexp "[0-9]";;
let newline_regex = Str.regexp "\n";;
let space_regex = Str.regexp " ";;

type token_data = {lineno: int; value: string};;
type token =
   | T_Open_Brace of token_data
   | T_Close_Brace of token_data
   | T_Print of token_data
   | T_Open_Paren of token_data
   | T_Close_Paren of token_data
   | T_While of token_data
   | T_Id of token_data
   | T_If of token_data
   | T_Int of token_data
   | T_String of token_data
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
;;

let rec join lst join_char = match lst with
    | []  -> ""
    | x::[]  -> x
    | x::xs -> x ^ join_char ^ (join xs join_char)
;;

let tokenize word lineno = match word with
    | "{" -> T_Open_Brace {lineno=lineno; value="{"}
    | "}" -> T_Close_Brace {lineno=lineno; value="}"}
    | "print" -> T_Print {lineno=lineno; value="print"}
    | "(" -> T_Open_Paren {lineno=lineno; value="("}
    | ")" -> T_Close_Paren {lineno=lineno; value=")"}
    | "while" -> T_While {lineno=lineno; value="while"}
    | "if" -> T_If {lineno=lineno; value="if"}
    | "int" -> T_Int {lineno=lineno; value="int"}
    | "string" -> T_String {lineno=lineno; value="string"}
    | "boolean" -> T_Boolean {lineno=lineno; value="boolean"}
    | "=" -> T_Assignment {lineno=lineno; value="="}
    | "==" -> T_Equality {lineno=lineno; value="=="}
    | "!=" -> T_Inequality {lineno=lineno; value="!="}
    | "false" -> T_False {lineno=lineno; value="false"}
    | "true" -> T_True {lineno=lineno; value="true"}
    | "+" -> T_Plus {lineno=lineno; value="+"}
    | "\"" -> T_Double_Quote {lineno=lineno; value="\""}
    | "$" -> T_Dollar_Sign {lineno=lineno; value="$"}
    | str when Str.string_match id_regex str 0 -> T_Id {lineno=lineno; value=str}
    (*| str when Str.string_match char_regex str 0 -> T_char {lineno=lineno; value=str} *)
    | str when Str.string_match digit_regex str 0 -> T_Digit {lineno=lineno; value=str}
    | x -> 
            Bolt.Logger.log "logger" Bolt.Level.ERROR ("Unrecognized token '"^ x ^ "' on line " ^ (string_of_int lineno)) ~file: "lex";
            raise (UnrecognizedTokenError (x, lineno))
;;

let split_on_newline str = Str.split newline_regex str;;
let split_on_space str = Str.split space_regex str;;
let lex str =
    (* ["{"; "print ( 3 )"; "}"; "$"] *)
    let list_of_strings_by_line = split_on_newline str in
    (* [["{"]; ["print"; "("; "3"; ")";] ["}"]; ["$"]] *)
    let list_of_lists_of_string_by_line_by_space = List.map (split_on_space) list_of_strings_by_line in
    let apply_tokenize_for_a_line lineno lst = List.map (fun x -> tokenize x lineno) lst in
    (* We use mapi because it actually uses the index as the first parameter *)
    let apply_tokenize_to_all lst = List.mapi apply_tokenize_for_a_line list_of_lists_of_string_by_line_by_space in
    let tokens_by_line_by_word = apply_tokenize_to_all list_of_lists_of_string_by_line_by_space in
    List.flatten tokens_by_line_by_word
;;
