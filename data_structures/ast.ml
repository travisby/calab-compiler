type ast =
    | Program of ast * Utils.pos
    | Block of ast list * Utils.pos
    | Print_Statement of ast * Utils.pos
    | Assignment_Statement of ast * ast * Utils.pos
    | Var_Decl of ast * ast * Utils.pos
    | While_Statement of ast * ast * Utils.pos
    | If_Statement of ast * ast * Utils.pos
    | Int of Utils.pos
    | Boolean of Utils.pos
    | String of Utils.pos
    | Id of char * Utils.pos
    | Plus of ast * ast * Utils.pos
    | Char_List of ast list * Utils.pos
    | Addition of ast * ast * Utils.pos
    | Equallity_Test of ast * ast * Utils.pos
    | Inequallity_Test of ast * ast * Utils.pos
    | Char of char * Utils.pos
    | Digit of int * Utils.pos
    | True of Utils.pos
    | False of Utils.pos
;;

let rec string_of_ast ?(indent="") ast =
    let tab = "    " in
    match ast with
    | Program (x, _) ->
            "\n"
            ^ indent
            ^ "Program"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ "\n"
    | Block (xs, _) ->
            let strings_of_xs = List.map (string_of_ast ~indent:(indent ^ tab)) xs in
            "\n"
            ^ indent
            ^ "Block"
            ^ (String.concat "" strings_of_xs)
    | Print_Statement (x, _) ->
            "\n"
            ^ indent
            ^ "print"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
    | Assignment_Statement (x, y, _) ->
            "\n"
            ^ indent
            ^ "="
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | Var_Decl (x, y, _) ->
            "\n"
            ^ indent
            ^ "Declared!"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | While_Statement (x, y, _) ->
            "\n"
            ^ indent
            ^ "While"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ "\n"
            ^ indent
            ^ "Do"
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | If_Statement (x, y, _) ->
            "\n"
            ^ indent
            ^ "If"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ "\n"
            ^ indent
            ^ "Then"
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | Int _ ->
            "\n"
            ^ indent
            ^ "int"
    | Boolean _ ->
            "\n"
            ^ indent
            ^ "boolean"
    | String _ ->
            "\n"
            ^ indent
            ^ "string"
    | Id (x, _) ->
            "\n"
            ^ indent
            ^ Char.escaped x
    | Char_List (_, _) -> (*TODO *) raise Not_found
    | Addition (x, y, _) ->
            "\n"
            ^ indent
            ^ "+"
            ^ (string_of_ast ~indent:(indent ^ tab) x)
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | Equallity_Test (x, y, _) ->
            "\n"
            ^ indent
            ^ "=="
            ^ (string_of_ast ~indent:(indent ^ tab) x) 
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | Inequallity_Test (x, y, _) ->
            "\n"
            ^ indent
            ^ "!="
            ^ (string_of_ast ~indent:(indent ^ tab) x) 
            ^ (string_of_ast ~indent:(indent ^ tab) y)
    | Char (x, _) ->
            "\n"
            ^ indent
            ^ Char.escaped x
    | Digit (d, _) ->
            "\n"
            ^ indent
            ^ string_of_int d
    | True _ ->
            "\n"
            ^ indent
            ^ "true"
    | False _ ->
            "\n"
            ^ indent
            ^ "false"
    | _ -> raise Not_found
;;

