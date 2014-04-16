let file_name = Array.get Sys.argv 1 in
let file_string = Utils.file_name_to_string file_name in
print_endline "Started compiler";
try Semantics.analyze (Parse.parse (Lex.lex file_string))
with x -> match x with
    |  Lex.UnrecognizedTokenError (token, lineno, charno) ->
            let file_by_lines = Str.split (Str.regexp "\n") file_string in
            let our_line = List.nth file_by_lines lineno in
            let karet_at = String.concat "" [String.make charno ' '; "^"] in
            print_endline "--------------------------";
            print_endline ("Unrecognized character " ^ (token) ^ " located on line " ^ (string_of_int lineno));
            print_endline our_line;
            print_endline karet_at;
            print_endline "--------------------------";
            raise x
    | Symbol_table.Already_Exists_In_table ->
            print_endline "--------------------------";
            print_endline "Item already exists in table";
            raise x
    | Symbol_table.Does_Not_Exist_In_Table ->
            print_endline "--------------------------";
            print_endline "Undeclared variable";
            raise x
    | Semantics.Type_Error ->
            print_endline "--------------------------";
            print_endline "Type Error";
            raise x
    | x ->
        print_newline ();
        print_string "Cannot recover.  Exitting";
        print_newline ();
        raise x
;;
print_endline "Ended compiler";
