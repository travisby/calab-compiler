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

let rec string_of_ast ast = match ast with
    | Program _ -> (* TODO *) raise Not_found
    | Block _ -> (* TODO *) raise Not_found
    | Print_Statement _ -> (* TODO *) raise Not_found
    | Assignment_Statement (x, y) -> (string_of_ast x) ^ " = " ^ (string_of_ast y)
    | Var_Decl (x, y) -> (string_of_ast x) ^ " " ^ (string_of_ast y)
    | While_Statement _ -> (*TODO *) raise Not_found
    | If_Statement _ -> (*TODO *) raise Not_found
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

