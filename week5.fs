let rec mixMap f x y =
    match x, y with
    | [], [] -> []
    | [], _ -> failwith ("unequal length")
    | _, [] -> failwith ("unequal length")
    | x :: [], y :: [] -> [ f (x, y) ]
    | x :: xs, y :: ys -> f (x, y) :: mixMap f xs ys

mixMap (fun f -> f) [ 1; 2; 3 ] [ 4; 5; 6 ]
//general type is f: (a'*'b -> c') x: a'list -> y: b'list -> (a' * b')list


let g x f = f x

// Take vector of pairs and return pair of vectors
let rec unmixMap f g x =
    match x with
    | (a, b) :: xs ->
        let (c, d) = unmixMap f g xs
        (f a :: c, g b :: d)
    | _ -> ([], [])

unmixMap (fun f -> f) (fun g -> g) [ (1, 5); (2, 4); (3, 3); (4, 2); (5, 1) ]
// (* general type is f: a' -> c' g: b' -> d' x: (a' * c')list -> (b'list * d'list) *


let myFunction x y = x

let rec f g =
    function
    | [] -> []
    | x :: xs -> g x :: f (fun y -> g (g y)) xs

let myFunction2 x f = f x



let pred x p =
    match x with
    | x when p x -> x
    | _ -> 0

let sumPred (x, p) =
    List.fold
        (fun s l ->
            match l with
            | x when p x -> s + 1
            | _ -> s)
        0
        x

let sumPredBack (x, p) =
    List.foldBack
        (fun l s ->
            match l with
            | x when p x -> s + 1
            | _ -> s)
        x
        0
