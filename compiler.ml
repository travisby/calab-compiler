open Symbol_table;;
open Utils;;
open Semantics;;

let log_info = Log.log_info_func "compiler";;

let file_name = Array.get Sys.argv 1 in
let file_string = Utils.file_name_to_string file_name in
print_endline "Started compiler";
try
    let tokens = Lex.lex file_string in
    let cst = Parse.parse tokens in
    let cst_string = Cst.string_of_cst cst in
    log_info "Printing the CST";
    log_info cst_string;
    let ast = Semantics.analyze cst in
    let ast_string = Ast.string_of_ast ast in
    log_info "Printing the AST";
    log_info ast_string
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
    | Symbol_table.Already_Exists_In_table (symbol1, symbol2) ->
            let line = symbol2.pos.lineno in
            let charno = symbol2.pos.charno in
            let file_by_lines = Str.split (Str.regexp "\n") file_string in
            let our_line = List.nth file_by_lines line in
            let karet_at = String.concat "" [String.make charno ' '; "^"] in
            print_endline "--------------------------";
            print_endline ("Redeclared variable " ^  "in same scope. located on line " ^ (string_of_int line));
            print_endline our_line;
            print_endline karet_at;
            print_endline "--------------------------";
            raise x
    | Symbol_table.Does_Not_Exist_In_Table symbol ->
            let line = symbol.pos.lineno in
            let charno = symbol.pos.charno in
            let file_by_lines = Str.split (Str.regexp "\n") file_string in
            let our_line = List.nth file_by_lines line in
            let karet_at = String.concat "" [String.make charno ' '; "^"] in
            print_endline "--------------------------";
            print_endline ("Undeclared variable " ^ "located on line " ^ (string_of_int line));
            print_endline our_line;
            print_endline karet_at;
            print_endline "--------------------------";
            raise x
    | Semantics.Type_Error (obj1, operator, obj2)->
            let line = operator.pos.lineno in
            let charno = operator.pos.charno in
            let file_by_lines = Str.split (Str.regexp "\n") file_string in
            let our_line = List.nth file_by_lines line in
            let karet_at = String.concat "" [String.make charno ' '; "^"] in
            print_endline "--------------------------";
            print_endline ("Trying to " ^ (operator.str) ^ " on incompatible types " ^ " on line " ^ (string_of_int line));
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
print_endline "Ended compiler";
