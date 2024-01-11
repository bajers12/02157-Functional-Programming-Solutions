// HR 1.5
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> n + fib (n - 1)
    | n when n > 0 -> fib (n - 1) + fib (n - 2)
    | _ -> failwith "n must be positive"

// HR 2.2

let rec pow s n =
    match n with
    | 0 -> ""
    | n when n > 0 -> s + pow s (n - 1)
    | _ -> failwith "n must be positive"


//  HR 2.8

let rec binomialCof n k =
    match n, k with
    | (row, 0) -> 1
    | (row, col) when col = row -> 1
    | (row, col) when col > row -> failwith "col must be less than row"
    | _ -> binomialCof (n - 1) (k - 1) + binomialCof (n - 1) k


// HR 4.7
let rec multiplicity x xs =
    match xs with
    | [] -> 0
    | y :: tail when y = x -> 1 + multiplicity x tail
    | y :: tail -> multiplicity x tail
