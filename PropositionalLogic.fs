// part 1
type Prop<'a> =
    | A of 'a
    | Dis of Prop<'a> * Prop<'a>
    | Con of Prop<'a> * Prop<'a>
    | Neg of Prop<'a>

let rec sem p asg =
    match p with
    | A(x) when asg = x -> true
    | Dis(p1, p2) -> (sem p1 asg) || (sem p2 asg)
    | Con(p1, p2) -> (sem p1 asg) && (sem p2 asg)
    | Neg(p1) -> not (sem p1 asg)
    | _ -> false

// part 2

let rec toNmf p =
    match p with
    | A(x) -> A(x)
    | Dis(p1, p2) -> Dis((toNmf p1), (toNmf p2))
    | Con(p1, p2) -> Con((toNmf p1), (toNmf p2))
    | Neg(p1) ->
        match p1 with
        | Neg(p2) -> toNmf p2
        | Dis(p2, p3) -> Con((toNmf (Neg(p2))), (toNmf (Neg(p3))))
        | Con(p2, p3) -> Dis((toNmf (Neg(p2))), (toNmf (Neg(p3))))
        | _ -> Neg(p1)



(Neg(Con(A "a", A "b")))

let rec sum =
    function
    | [] -> 0
    | x :: xs -> (sum xs) + x

let rec sum2 =
    function
    | [] -> 0
    | x :: xs -> x + sum xs

let rec bigListA n xs =
    if n = 0 then xs else bigListA (n - 1) (1 :: xs)

let x = bigListA 1000000 []

#time

sum2 x
