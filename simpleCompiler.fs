type Exp =
    | C of int
    | X
    | Add of Exp * Exp
    | Sub of Exp * Exp
    | Minus of Exp
    | Abs of Exp

let rec sem (e: Exp) (x: int) =
    match e with
    | C(c) -> c
    | X -> x
    | Add(e1, e2) -> (sem e1 x) + (sem e2 x)
    | Sub(e1, e2) -> (sem e1 x) - (sem e2 x)
    | Minus(e1) -> -(sem e1 x)
    | Abs(e1) -> abs (sem e1 x)



type Instruction =
    | ADD
    | SUB
    | SIGN
    | ABS
    | PUSH of int


type Stack = int list

let intpInstr (s: Stack) (i: Instruction) : Stack =
    match s, i with
    | (x1 :: x2 :: xs, ADD) -> (x2 + x1) :: xs
    | (x1 :: x2 :: xs, SUB) -> (x2 - x1) :: xs
    | (x1 :: xs, SIGN) -> (-x1) :: xs
    | (x1 :: xs, ABS) -> (abs x1) :: xs
    | (xs, PUSH(x)) -> x :: xs
    | _ -> failwith "Illegal operation"


let exec (i: Instruction list) =
    let s = List.fold (fun acc x -> intpInstr acc x) [] i
    match s with
    | [] -> failwith "stack is empty"
    | s1 :: srest -> s1




let rec compile (e: Exp) (x: int) : Instruction list =
    match e with
    | C(c) -> [ PUSH(c) ]
    | X -> [ PUSH(x) ]
    | Add(e1, e2) -> (compile e1 x) @ (compile e2 x) @ [ ADD ]
    | Sub(e1, e2) -> (compile e1 x) @ (compile e2 x) @ [ SUB ]
    | Minus(e1) -> (compile e1 x) @ [ SIGN ]
    | Abs(e1) -> (compile e1 x) @ [ ABS ]

let rec red =
    function
    | C(c) -> C(c)
    | X -> X
    | Add(e1, e2) ->
        match (red e1, red e2) with
        | (C(c1), C(c2)) -> C(c1 + c2)
        | (C(0), e) -> e
        | (e, C(0)) -> e
        | (e3, e4) -> Add(e3, e4)
    | Sub(e1, e2) ->
        match (red e1, red e2) with
        | (C(c1), C(c2)) -> C(c1 - c2)
        | (e, C(0)) -> e
        | (C(0), e) -> Minus(e)
        | (e3, Minus(e4)) -> Add(e3, e4)
        | (e3, e4) -> Sub(e3, e4)
    | Minus(e) ->
        match red e with
        | C(c1) -> C(-c1)
        | Minus(e1) -> red e1
        | x -> Minus(x)
    | Abs(c) ->
        match red c with
        | C(x) -> C(abs x)
        | Minus(e) -> Abs(red e)
        | Abs(e) -> Abs(red e)
        | x -> Abs(x)
red (Add(Minus(Add(C 3, C 7)), Abs(Sub(C 4, X))));;


let f p xs =
    List.filter(fun x -> p x) xs;;


