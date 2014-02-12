open Str

exception UnrecognizedTokenError

let id_regex = Str.regexp "[a-zA-Z][a-zA-z0-9_]*"
let char_regex = Str.regexp "[a-zA-Z]"
let digit_regex = Str.regexp "[0-9]"
type token_data = {lineno: int; value: string}
type token =
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
   | T_doublequote of token_data

let rec join lst join_char = match lst with
    | []  -> ""
    | x::[]  -> x
    | x::xs -> x ^ join_char ^ (join xs join_char)

let tokenize word lineno = match word with
    | "{" -> T_OpenBrace {lineno=lineno; value="{"}
    | "}" -> T_CloseBrace {lineno=lineno; value="}"}
    | "print" -> T_print {lineno=lineno; value="print"}
    | "(" -> T_OpenParen {lineno=lineno; value="("}
    | ")" -> T_CloseParen {lineno=lineno; value=")"}
    | "while" -> T_while {lineno=lineno; value="while"}
    | "if" -> T_if {lineno=lineno; value="if"}
    | "int" -> T_int {lineno=lineno; value="int"}
    | "string" -> T_string {lineno=lineno; value="string"}
    | "boolean" -> T_boolean {lineno=lineno; value="boolean"}
    | "=" -> T_assignment {lineno=lineno; value="="}
    | "==" -> T_equality {lineno=lineno; value="=="}
    | "!=" -> T_inequality {lineno=lineno; value="!="}
    | "false" -> T_false {lineno=lineno; value="false"}
    | "true" -> T_true {lineno=lineno; value="true"}
    | "+" -> T_plus {lineno=lineno; value="+"}
    | "\"" -> T_doublequote {lineno=lineno; value="\""}
    | str when Str.string_match id_regex str 0 -> T_id {lineno=lineno; value=str}
    (*| str when Str.string_match char_regex str 0 -> T_char {lineno=lineno; value=str} *)
    | str when Str.string_match digit_regex str 0 -> T_digit {lineno=lineno; value=str}
    | _ -> raise UnrecognizedTokenError
;;

let addSpaceToOccurrence str item = join (Str.split (Str.regexp item) str) (" " ^ item ^ " ")
