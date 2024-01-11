type Poly = int list

let rec isLegal (xs: int list) : bool =
    match xs with
    | [] -> true
    | [ x ] when x = 0 -> false
    | x :: xs -> isLegal xs


let rec reverseList (xs: int list) : int list =
    match xs with
    | [] -> []
    | x :: xs -> reverseList xs @ [ x ]

let ofList (xs: int list) : Poly =
    let rec ofList' xs =
        match xs with
        | [] -> []
        | x :: xs when x = 0 -> ofList' xs
        | _ -> xs

    let xs = reverseList xs
    let xs = ofList' xs
    reverseList xs


let add (x: Poly) (y: Poly) : Poly =
    let rec add' x y =
        match x, y with
        | ([], _) -> y
        | (_, []) -> x
        | (x :: xs, y :: ys) -> x + y :: add' xs ys

    let l = add' x y
    if isLegal l then l else ofList l


let mulC (n: int) (xs: Poly) : Poly =
    let rec mulC' n xs =
        match xs with
        | [] -> []
        | x :: xs -> n * x :: mulC' n xs

    let l = mulC' n xs
    if isLegal l then l else ofList l

let sub (xs: Poly) (ys: Poly) : Poly =
    let rec sub' xs ys =
        match xs, ys with
        | ([], []) -> []
        | ([], y :: ys) -> -y :: sub' [] ys
        | (x :: xs, []) -> x :: xs
        | (x :: xs, y :: ys) -> x - y :: sub' xs ys

    let l = sub' xs ys
    if isLegal l then l else ofList l


let mulX (xs: Poly) : Poly = ofList (0 :: xs)


let mul (xs: Poly) (ys: Poly) : Poly =
    let rec mul' xs ys =
        match xs, ys with
        | ([], _) -> []
        | (_, []) -> []
        | (x :: xs, ys) -> add (mulC x ys) (mulX (mul' xs ys))

    let l = mul' xs ys
    if isLegal l then l else ofList l


let rec eval (x: int) (y: Poly) : int =
    match y with
    | [] -> 0
    | y :: ys -> y + x * eval x ys


let toString (xs: Poly) : string =
    let rec toString' xs n =
        match xs with
        | [] -> ""
        | x :: xs when x = 0 -> toString' xs (n + 1)
        | x :: xs when n = 0 -> x.ToString() + toString' xs (n + 1)
        | x :: xs when x > 0 && n = 1 -> "+" + x.ToString() + "x" + toString' xs (n + 1)
        | x :: xs when n = 1 -> x.ToString() + "x" + toString' xs (n + 1)
        | x :: xs when x > 0 -> "+" + x.ToString() + "x^" + n.ToString() + toString' xs (n + 1)
        | x :: xs -> (x).ToString() + "x^" + n.ToString() + toString' xs (n + 1)

    let xs = ofList xs
    "The polynomial is: " + toString' xs 0


let derivative (xs: Poly) : Poly =
    let rec derivative' xs n =
        match xs with
        | [] -> []
        | x :: xs when n = 0 -> derivative' xs (n + 1)
        | x :: xs -> x * n :: derivative' xs (n + 1)

    let l = derivative' xs 0
    if isLegal l then l else ofList l





let compose (xs: Poly) (ys: Poly) : Poly =
    let rec recmul ys n =
        match n with
        | 1 -> ys
        | _ -> mul ys (recmul ys (n - 1))

    let rec compose' xs ys n =
        match xs with
        | [] -> []
        | x :: xs when n = 0 -> add [ x ] (compose' xs ys (n + 1))
        | x :: xs -> add (mulC x (recmul ys n)) (compose' xs ys (n + 1))

    let l = compose' xs ys 0
    if isLegal l then l else ofList l






// part 4
type Degree =
    | MinusInf
    | Fin of int


let degree (p: Poly) : Degree =
    if isLegal p then
        match List.length p with
        | 0 -> MinusInf
        | x -> Fin(x - 1)
    else
        match List.length (ofList p) with
        | 0 -> MinusInf
        | x -> Fin(x - 1)

let addD (d1: Degree) (d2: Degree) : Degree =
    match (d1, d2) with
    | (MinusInf, _) -> MinusInf
    | (_, MinusInf) -> MinusInf
    | (Fin(x), Fin(y)) -> Fin(x + y)











let ofList (x: int list) : Poly =

    let rec ofList' x =
        match x with
        | [] -> []
        | xh :: xt when xh = 0 -> ofList' xt
        | xh :: xt -> xh :: xt

    List.rev (ofList' (List.rev x))
