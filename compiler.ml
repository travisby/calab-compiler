let file_name = Array.get Sys.argv 1 in
let file_string = Utils.file_name_to_string file_name in
Bolt.Logger.log "logger" Bolt.Level.TRACE "started" ~file: "compiler";
try Parse.parse (Lex.lex file_string)
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
    | x ->
        print_newline ();
        print_string "Cannot recover.  Exitting";
        print_newline ();
        raise x
;;
Bolt.Logger.log "logger" Bolt.Level.TRACE "finished" ~file: "compiler";
