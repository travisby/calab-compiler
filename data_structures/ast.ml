type ast =
    | Program of ast
    | Block of ast list
    | Print_Statement of ast
    | Assignment_Statement of ast * ast
    | Var_Decl of ast * ast
    | While_Statement of ast * ast
    | If_Statement of ast * ast
    | Int
    | Boolean
    | String
    | Id of char
    | Plus of ast * ast
    | Char_List of ast list
    | Addition of ast * ast
    | Equallity_Test of ast * ast
    | Inequallity_Test of ast * ast
    | Char of char
    | Digit of int
    | True | False
;;

let rec string_of_ast ?(indent="") ast = match ast with
    | Program x ->
            "\n"
            ^ indent
            ^ "Program"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
    | Block xs ->
            let strings_of_xs = List.map (string_of_ast ~indent:(indent ^ " ")) xs in
            "\n"
            ^ indent
            ^ "Block"
            ^ (String.concat "" strings_of_xs)
    | Print_Statement x ->
            "\n"
            ^ indent
            ^ "Print"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
    | Assignment_Statement (x, y) -> (string_of_ast x) ^ " = " ^ (string_of_ast y)
    | Var_Decl (x, y) -> (string_of_ast x) ^ " " ^ (string_of_ast y)
    | While_Statement (x, y) ->
            "\n"
            ^ indent
            ^ "While"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
            ^ "\n"
            ^ indent
            ^ "Do"
            ^ (string_of_ast ~indent:(indent ^ "    ") y)
    | If_Statement (x, y) ->
            "\n"
            ^ indent
            ^ "If"
            ^ (string_of_ast ~indent:(indent ^ "    ") x)
            ^ "\n"
            ^ indent
            ^ "Then"
            ^ (string_of_ast ~indent:(indent ^ "    ") y)
    | Int -> "Int"
    | Boolean -> "Boolean"
    | String -> "String"
    | Id x -> Char.escaped x
    | Char_List _ -> (*TODO *) raise Not_found
    | Addition (x, y) -> (string_of_ast x) ^ " + " ^ (string_of_ast y)
    | Equallity_Test (x, y) -> (string_of_ast x) ^ " == " ^ (string_of_ast y)
    | Inequallity_Test (x, y) -> (string_of_ast x) ^ " != " ^ (string_of_ast y)
    | Char x -> Char.escaped x
    | Digit d -> string_of_int d
    | True -> "true"
    | False -> "false"
    | _ -> raise Not_found
;;

