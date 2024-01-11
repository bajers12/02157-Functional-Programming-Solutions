// MergeSort

let rec merge (xs, ys) =
    match xs, ys with
    | [], [] -> []
    | x :: xs, [] -> x :: xs
    | [], y :: ys -> y :: ys
    | x :: xs, y :: ys when x < y -> x :: merge (xs, y :: ys)
    | x :: xs, y :: ys -> y :: merge (x :: xs, ys)

let rec split xs =
    match xs with
    | [] -> ([], [])
    | x :: [] -> ([ x ], [])
    | x :: y :: xs ->
        let (a, b) = split xs
        (x :: a, y :: b)

let rec mergeSort xs =
    match xs with
    | [] -> []
    | x :: [] -> [ x ]
    | _ ->
        let (a, b) = split xs
        merge (mergeSort a, mergeSort b)
