exception UnrecognizedTokenError of string * int;;
exception UnrecognizedTokenInStringError of string;;
exception CannotContainUnderscoreError;;
let id_regex = Str.regexp "^[a-z]$";;
let char_regex = Str.regexp "^[a-z ]$";;
let digit_regex = Str.regexp "^[0-9]$";;
let newline_regex = Str.regexp "\n";;
let whitespace_regex = Str.regexp "[\t\n\r ]";;
let space_regex = Str.regexp " ";;
let quote_regex = Str.regexp "\"";;

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
;;

let log_trace = Log.log_trace_func "lex"
let log_error = Log.log_error_func "lex"
let log_warn = Log.log_warn_func "lex"

let lex str =
    let char_list = Utils.string_of_char_list str in
    let on_char next_char lst =
        (* 
         * We use this to know whether or not we're in a string literal.  If we
         * are, we shouldn't combine characters
         *)
        let odd_quotes = Utils.odd (List.length (List.filter (function | "\"" -> true | _ -> false) lst)) in
        if
            List.length lst > 0
        then
            match (Utils.list_last lst) with
            (* > 1 character operators *)
            | "!" when next_char = '=' -> (Utils.list_init lst) @ ["!="]
            | "=" when next_char = '=' -> (Utils.list_init lst) @ ["=="]
            (* print permutations... *)
            | "p" when next_char = 'r' && not odd_quotes -> (Utils.list_init lst) @ ["pr"]
            | "pr" when next_char = 'i' -> (Utils.list_init lst) @ ["pri"]
            | "pri" when next_char = 'n' -> (Utils.list_init lst) @ ["prin"]
            | "prin" when next_char = 't' -> (Utils.list_init lst) @ ["print"]
            (* while permutations... *)
            | "w" when next_char = 'h' && not odd_quotes -> (Utils.list_init lst) @ ["wh"]
            | "wh" when next_char = 'i' -> (Utils.list_init lst) @ ["whi"]
            | "whi" when next_char = 'l' -> (Utils.list_init lst) @ ["whil"]
            | "whil" when next_char = 'e' -> (Utils.list_init lst) @ ["while"]
            (* if permutations... *)
            | "i" when next_char = 'f' && not odd_quotes -> (Utils.list_init lst) @ ["if"]
            (* int permutations... *)
            | "i" when next_char = 'n' && not odd_quotes -> (Utils.list_init lst) @ ["in"]
            | "in" when next_char = 't' -> (Utils.list_init lst) @ ["int"]
            (* string permutations... *)
            | "s" when next_char = 't' && not odd_quotes -> (Utils.list_init lst) @ ["st"]
            | "st" when next_char = 'r' -> (Utils.list_init lst) @ ["str"]
            | "str" when next_char = 'i' -> (Utils.list_init lst) @ ["stri"]
            | "stri" when next_char = 'n' -> (Utils.list_init lst) @ ["strin"]
            | "strin" when next_char = 'g' -> (Utils.list_init lst) @ ["string"]
            (* boolean permutations *)
            | "b" when next_char = 'o' && not odd_quotes -> (Utils.list_init lst) @ ["bo"]
            | "bo" when next_char = 'o' -> (Utils.list_init lst) @ ["boo"]
            | "boo" when next_char = 'l' -> (Utils.list_init lst) @ ["bool"]
            | "bool" when next_char = 'e' -> (Utils.list_init lst) @ ["boole"]
            | "boole" when next_char = 'a' -> (Utils.list_init lst) @ ["boolea"]
            | "boolea" when next_char = 'n' -> (Utils.list_init lst) @ ["boolean"]
            (* false permutations *)
            | "f" when next_char = 'a' && not odd_quotes -> (Utils.list_init lst) @ ["fa"]
            | "fa" when next_char = 'l' -> (Utils.list_init lst) @ ["fal"]
            | "fal" when next_char = 's' -> (Utils.list_init lst) @ ["fals"]
            | "fals" when next_char = 'e' -> (Utils.list_init lst) @ ["false"]
            (* true permutations *)
            | "t" when next_char = 'r' && not odd_quotes -> (Utils.list_init lst) @ ["tr"]
            | "tr" when next_char = 'u' -> (Utils.list_init lst) @ ["tru"]
            | "tru" when next_char = 'e' -> (Utils.list_init lst) @ ["true"]
            (*
             * what if building up our strings was wrong?
             * We will want to explode the previous built-up string
             *
             * But only if we are not actually... complete
             *)
            | x when String.length x > 1
            && not (List.mem x ["!="; "=="; "print"; "while"; "if"; "int"; "string"; "boolean"; "false"; "true"]) ->
                    let piece1 = Utils.list_init lst in
                    let piece2 = Utils.string_of_string_list x in
                    let piece3 = [Utils.char_of_string next_char] in
                    piece1 @ piece2 @ piece3
            (* Base case... just create a new element *)
            | _ -> lst @ [Utils.char_of_string next_char]
        else
            lst @ [Utils.char_of_string next_char]
    in
    let token_possibles = List.rev (List.fold_right (on_char) (List.rev char_list) []) in
    let on_token_possible next_token_possible tokens =
        log_trace ("Looking at " ^ next_token_possible);
        (* special token used for lookup later *)
        let ds = T_Dollar_Sign {lineno=(-1); value="$"} in
        let odd_quotes = Utils.odd (List.length (List.filter (function | T_Double_Quote _ -> true | _ -> false) tokens)) in
        (* TODO handle lineno *)
        let token_data = {lineno=0; value=next_token_possible} in
        if
            List.mem ds tokens
        then
            begin
                log_warn ("Extra character after $... Ignoring: " ^ next_token_possible);
                tokens
            end
        else
            match next_token_possible with
                | "{" -> tokens @ [T_Open_Brace token_data]
                | "}" -> tokens @ [T_Close_Brace token_data]
                | "print" -> tokens @ [T_Print token_data]
                | "(" -> tokens @ [T_Open_Paren token_data]
                | ")" -> tokens @ [T_Close_Paren token_data]
                | "while" -> tokens @ [T_While token_data]
                | "if" -> tokens @ [T_If token_data]
                | "int" -> tokens @ [T_Int token_data]
                | "string" -> tokens @ [T_String token_data]
                | "boolean" -> tokens @ [T_Boolean token_data]
                | "=" -> tokens @ [T_Assignment token_data]
                | "==" -> tokens @ [T_Equality token_data]
                | "!=" -> tokens @ [T_Inequality token_data]
                | "true" -> tokens @ [T_True token_data]
                | "false" -> tokens @ [T_False token_data]
                | "+" -> tokens @ [T_Plus token_data]
                | "\"" -> tokens @ [T_Double_Quote token_data]
                | "$" -> tokens @ [ds]
                | x when Str.string_match digit_regex x 0 -> tokens @ [T_Digit token_data]
                | x when Str.string_match id_regex x 0 && not odd_quotes  -> tokens @ [T_Id token_data]
                | x when Str.string_match whitespace_regex x 0 -> tokens
                | x when (Str.string_match char_regex x 0 && odd_quotes) -> tokens @ [T_Char token_data]
                (* TODO handle lineno *)
                | _ -> raise (UnrecognizedTokenError (next_token_possible, 0))
    in
    List.fold_right on_token_possible token_possibles []
;;

let token_as_string token = match token with
    | T_Open_Brace x -> "{"
    | T_Close_Brace x -> "}"
    | T_Print x -> "print"
    | T_Open_Paren x ->  "("
    | T_Close_Paren x -> ")"
    | T_While x -> "while"
    | T_Id x -> "id " ^ x.value
    | T_If x -> "if"
    | T_Int x -> "int"
    | T_String x -> "string"
    | T_Char_List x -> "char list" ^ x.value
    | T_Boolean x -> "boolean"
    | T_Char x -> "char " ^ x.value
    | T_Digit x -> "digit " ^ x.value
    | T_Assignment x -> "="
    | T_Equality x -> "=="
    | T_Inequality x -> "!="
    | T_False x -> "false"
    | T_True x -> "true"
    | T_Plus x -> "+"
    | T_Double_Quote x -> "\""
    | T_Dollar_Sign x -> "$"
;;
