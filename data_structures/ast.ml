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

let rec string_of_ast ?(indent="") ast = match ast with
    | Program (x, _) ->
            "\n"
            ^ indent
            ^ "Program"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
    | Block (xs, _) ->
            let strings_of_xs = List.map (string_of_ast ~indent:(indent ^ " ")) xs in
            "\n"
            ^ indent
            ^ "Block"
            ^ (String.concat "" strings_of_xs)
    | Print_Statement (x, _) ->
            "\n"
            ^ indent
            ^ "Print"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
    | Assignment_Statement (x, y, _) -> (string_of_ast x) ^ " = " ^ (string_of_ast y)
    | Var_Decl (x, y, _) -> (string_of_ast x) ^ " " ^ (string_of_ast y)
    | While_Statement (x, y, _) ->
            "\n"
            ^ indent
            ^ "While"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
            ^ "\n"
            ^ indent
            ^ "Do"
            ^ (string_of_ast ~indent:(indent ^ "    ") y)
    | If_Statement (x, y, _) ->
            "\n"
            ^ indent
            ^ "If"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
            ^ "\n"
            ^ indent
            ^ "Then"
            ^ (string_of_ast ~indent:(indent ^ "    ") y)
    | Int _ -> "Int"
    | Boolean _ -> "Boolean"
    | String _ -> "String"
    | Id (x, _) -> Char.escaped x
    | Char_List (_, _) -> (*TODO *) raise Not_found
    | Addition (x, y, _) -> (string_of_ast x) ^ " + " ^ (string_of_ast y)
    | Equallity_Test (x, y, _) -> (string_of_ast x) ^ " == " ^ (string_of_ast y)
    | Inequallity_Test (x, y, _) -> (string_of_ast x) ^ " != " ^ (string_of_ast y)
    | Char (x, _) -> Char.escaped x
    | Digit (d, _) -> string_of_int d
    | True _ -> "true"
    | False _ -> "false"
    | _ -> raise Not_found
;;

