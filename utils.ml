let rec join join_char lst = match lst with
    | []  -> ""
    | x::[]  -> x
    | x::xs -> x ^ join_char ^ (join join_char xs)
;;

let file_name_to_string file_name =
    let file = open_in file_name in
    let rec get_by_line ?lst:(lst=[]) file = 
        (* 
         * The input_line function will raise End_of_file
         * when we are at the end.
         * So we keep going deeper into the recursion until we hit this exception,
         * and then just return the already-built list
         *
         * Else we build it with the next line
         *)
        try get_by_line ~lst:(lst @ [input_line file]) file
        with End_of_file -> lst
    in
    join "\n" (get_by_line file)
