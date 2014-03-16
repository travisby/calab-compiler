exception UnrecognizedTokenError of string * int;;
exception UnrecognizedTokenInStringError of string;;
exception CannotContainUnderscoreError;;

let id_regex = Str.regexp "[a-z]";;
let char_regex = Str.regexp "[a-zA-Z]";;
let digit_regex = Str.regexp "[0-9]";;
let newline_regex = Str.regexp "\n";;
let whitespace_regex = Str.regexp "\\s+";;
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

let tokenize strs lineno word = match word with
    | "{" ->
            log_trace ("Found { token on line " ^ (string_of_int lineno));
            T_Open_Brace {lineno=lineno; value="{"}
    | "}" ->
            log_trace ("Found } token on line " ^ (string_of_int lineno));
            T_Close_Brace {lineno=lineno; value="}"}
    | "print" ->
            log_trace ("Found print token on line " ^ (string_of_int lineno));
            T_Print {lineno=lineno; value="print"}
    | "(" ->
            log_trace ("Found ( token on line " ^ (string_of_int lineno));
            T_Open_Paren {lineno=lineno; value="("}
    | ")" ->
            log_trace ("Found ) token on line " ^ (string_of_int lineno));
            T_Close_Paren {lineno=lineno; value=")"}
    | "while" ->
            log_trace ("Found while token on line " ^ (string_of_int lineno));
            T_While {lineno=lineno; value="while"}
    | "if" ->
            log_trace ("Found if token on line " ^ (string_of_int lineno));
            T_If {lineno=lineno; value="if"}
    | "int" ->
            log_trace ("Found int token on line " ^ (string_of_int lineno));
            T_Int {lineno=lineno; value="int"}
    | "string" ->
            log_trace ("Found string token on line " ^ (string_of_int lineno));
            T_String {lineno=lineno; value="string"}
    | "boolean" ->
            log_trace ("Found boolean token on line " ^ (string_of_int lineno));
            T_Boolean {lineno=lineno; value="boolean"}
    | "=" ->
            log_trace ("Found = token on line " ^ (string_of_int lineno));
            T_Assignment {lineno=lineno; value="="}
    | "==" ->
            log_trace ("Found == token on line " ^ (string_of_int lineno));
            T_Equality {lineno=lineno; value="=="}
    | "!=" ->
            log_trace ("Found != token on line " ^ (string_of_int lineno));
            T_Inequality {lineno=lineno; value="!="}
    | "false" ->
            log_trace ("Found false token on line " ^ (string_of_int lineno));
            T_False {lineno=lineno; value="false"}
    | "true" ->
            log_trace ("Found true token on line " ^ (string_of_int lineno));
            T_True {lineno=lineno; value="true"}
    | "+" ->
            log_trace ("Found + token on line " ^ (string_of_int lineno));
            T_Plus {lineno=lineno; value="+"}
    | "\"" ->
            log_trace ("Found \" token on line " ^ (string_of_int lineno));
            T_Double_Quote {lineno=lineno; value="\""}
    | "$" ->
            log_trace ("Found $ token on line " ^ (string_of_int lineno));
            T_Dollar_Sign {lineno=lineno; value="$"}
    | "_" ->
            log_trace ("Found a charlist on line " ^ (string_of_int lineno));
            T_Char_List {lineno=lineno; value=(Queue.pop strs)}
    | str when Str.string_match id_regex str 0 ->
            log_trace ("Found id token" ^ str ^ "on line " ^ (string_of_int lineno));
            T_Id {lineno=lineno; value=str}
    | str when Str.string_match digit_regex str 0 ->
            log_trace ("Found digit token" ^ str ^ "on line " ^ (string_of_int lineno));
            T_Digit {lineno=lineno; value=str}
    | x -> 
            log_error ("Unrecognized token '"^ x ^ "' on line " ^ (string_of_int lineno));
            raise (UnrecognizedTokenError (x, lineno))
;;

let split_on_newline str = Str.split newline_regex str;;
let split_on_space str = Str.split space_regex str;;
let split_on_quote str = Str.split quote_regex str;;

let get_string_queue str =
    (*
     * Because our program must not begin with ", we can guarantee
     * that every odd indexed element in our split list will be the contents of
     * a string!
     *
     * e.g., { "hello alan" } would be split to... ["{", "hello alan", "}]
     * We can just return every odd-indexed element, and rejoin
     *)
    let strings_by_quotes = split_on_quote str in
    let strings_in_str = Utils.get_odd_indexes strings_by_quotes in
    Utils.queue_from_list strings_in_str

let replace_strings str = 
    (*
     * Using the same technique as for get_string_queue,
     * We can isolate the strings, and replace them with an underscore,
     * rebuild the string, and return
     *)
    let strings_by_quotes = split_on_quote str in
    let strings_replaced = List.mapi (fun i x -> if Utils.odd i then x else "_") strings_by_quotes in
    if
        (Str.string_match (Str.regexp "[^a-z ]") str 0)
    then
        raise (UnrecognizedTokenInString (Str.matched_string str))
    else
        Utils.join " " strings_replaced


let lex str =
    (*
     * Without the knowledge that we are within quotes at a given time,
     * our tokenize function would be ambiguous.  We would not know whether
     * the current characters are creating an id, or a charlist
     *
     * To prevent this we search the full string for quotes
     * When we found a quote, we replace it with the _ character, 
     * something that should not already exist in the user's program.
     * We then place all of that replaced text into a queue strings_in_program
     *
     * The above gives us a state of always having a "one word" token to
     * tokenize, allowing us to split on quotes
     *
     * We make sure that is passed into our tokenize function, so any time our
     * keyword is found, we can then replace it with the value in the queue!
     *)

    (* ensure there is no _ already in the program *)
    if String.contains str '_' then raise CannotContainUnderscore else ();
    let my_reg = Str.regexp "==\\|=\\|[(){}$\"\\|\\+]" in
    let add_spaces_func str = (Str.global_replace my_reg " \\0 " str) in
    let remove_blanks_func = List.filter (fun x -> not (Str.string_match (Str.regexp "^$") x 0)) in
    let tokenize_by_line_func lineno queue = List.map (fun x -> tokenize queue lineno x) in

    let strings_in_program = get_string_queue str in
    let string_replaced = replace_strings str in

    let my_str = add_spaces_func string_replaced in
    let string_lines = Str.split newline_regex my_str in
    let string_lines_words = List.map (Str.split space_regex) string_lines in
    let trimmed = List.map (List.map String.trim) string_lines_words in
    let filtered = List.map (remove_blanks_func) trimmed in
    let token_lines = List.mapi (fun i -> tokenize_by_line_func  i strings_in_program) filtered in
    List.flatten token_lines
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
