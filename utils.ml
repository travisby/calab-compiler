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

let odd x = x mod 2 <> 0;;

let rec get_odd_indexes ?isOdd:(isOdd=false) lst = match lst, isOdd with
    | [], _ -> []
    | x::xs, true -> x :: (get_odd_indexes ~isOdd:false xs)
    | x::xs, false -> (get_odd_indexes ~isOdd:true xs)
;;

let queue_from_list lst =
    let queue = Queue.create () in
    let rec fill_queue lst = match lst with
        | [] -> ()
        | x::xs -> Queue.add x queue; fill_queue xs
    in
    fill_queue lst;
    queue
;;
