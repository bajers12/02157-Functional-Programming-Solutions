// Problem 1
type Team = string
type Goal = int
type Point = int
type Matches = int
type Score = Matches * Goal * Goal * Point
type TeamScore = Team * Score
type Standings = TeamScore list


let ss =
    [ ("T1", (3, 5, 1, 9))
      ("T3", (3, 4, 2, 4))
      ("T2", (3, 4, 4, 4))
      ("T4", (3, 0, 6, 0)) ]

// 1
let better sc1 sc2 =
    match sc1, sc2 with
    | (_, _, _, p1), (_, _, _, p2) when p1 > p2 -> true
    | (_, gs1, gc1, p1), (_, gs2, gc2, p2) when p1 = p2 && (gs1 - gc1) > (gs2 - gc2) -> true
    | _, _ -> false

let equal sc1 sc2 =
    match sc1, sc2 with
    | (_, gs1, gc1, p1), (_, gs2, gc2, p2) when p1 = p2 && (gs1 - gc1) = (gs2 - gc2) -> true
    | _, _ -> false

let betterOrEqual sc1 sc2 = (better sc1 sc2) || (equal sc1 sc2)

// 2
let rec listContains x xs =
    match xs with
    | [] -> false
    | (x', _) :: xs' when x = x' -> true
    | (x', _) :: xs' -> listContains x xs'

let rec descending' x xs =
    match xs with
    | [] -> true
    | (_, x') :: xs' when betterOrEqual x x' -> descending' x xs'
    | _ -> false

let properlyOrdered ss =
    let rec p1 ss =
        match ss with
        | [] -> true
        | (x, _) :: xs when listContains x xs -> false
        | _ :: xs -> p1 xs

    let rec p2 ss =
        match ss with
        | [] -> true
        | (_, x) :: xs when descending' x xs -> p2 xs
        | _ -> false

    (p1 ss) && (p2 ss)


// 3
let rec init ts =
    match ts with
    | [] -> []
    | x :: xs -> (x, (0, 0, 0, 0)) :: init xs

// 4
let extractScoreOf t ss =
    let rec extractScoreOf' t ss acc =
        match ss with
        | [] -> failwith $"{t} is not a team in the score standning"
        | (t', s) :: xs when t = t' -> (t, acc @ xs)
        | (t', s) :: xs -> extractScoreOf' t xs (acc @ [ (t', s) ])

    extractScoreOf' t ss []

// 5
let update sc g1 g2 =
    let (m, gs, gc, p) = sc

    if g1 > g2 then (m + 1, gs + g1, gc + g2, p + 3)
    else if g1 < g2 then (m + 1, gs + g1, gc + g2, p)
    else (m + 1, gs + g1, gc + g2, p + 1)
// 6
let rec insertTeamScore (t, sc) ss =
    match ss with
    | [] -> [ (t, sc) ]
    | (t', sc') :: xs when betterOrEqual sc sc' -> (t, sc) :: ss
    | (t', sc') :: xs -> (t', sc') :: (insertTeamScore (t, sc) xs)
// 7
let newStandings mr ss =
    let rec updateScores mr ss =
        match mr, ss with
        | _, [] -> []
        | (t1, t2, g1, g2), (x, sc) :: xs when t1 = x -> (x, (update sc g1 g2)) :: (updateScores mr xs) 
        | (t1, t2, g1, g2), (x, sc) :: xs when t2 = x -> (x, (update sc g2 g1)) :: (updateScores mr xs)
        | _, (x, sc) :: xs -> (x, sc) :: (updateScores mr xs)

    let rec newStanding' ss newS =
        match ss with
        | [] -> newS
        | x :: xs -> newStanding' xs (insertTeamScore x newS)

    newStanding' (updateScores mr ss) []



// problem 2

// 1
let rec partition p xs =
    match xs with
    | [] -> ([], [])
    | x :: rest ->
        let (xs1, xs2) = partition p rest
        if p x then (x :: xs1, xs2) else (xs1, x :: xs2)

// the type of partition is ('a->bool) -> 'a list -> ('a list)*('a list)
// it splits up a list into a tuple of 2 lists, one containing elements that satisfy p
// one that does not
// we can see return type is ('a list) * ('a list) because of the base case return value
// we make no operations on the heads, so therefore it must be a generic typed list
// we know p is a 'a -> bool, as it returns a bool as seen in the if statement
// 2nd input is a list, as we match it on a list


// 2
let a x = x >= 5
let b = [ 6; 8; 9; 1; 4; 2 ]
// 3
let partition p xs =
    let rec partition' p xs (acc1, acc2) =
        match xs with
        | [] -> (acc1, acc2)
        | x :: rest when p x -> partition' p rest (acc1 @ [ x ], acc2)
        | x :: rest -> partition' p rest (acc1, acc2 @ [ x ])

    partition' p xs ([], [])

// 4
let partition p xs =
    List.foldBack (fun x (acc1, acc2) -> if p x then (x :: acc1, acc2) else (acc1, x :: acc2)) xs ([], [])

// problem 3
// 1
type H<'d> = N of 'd * Children<'d>
and Children<'d> = H<'d> list

let c1 = N(true, [ N(true, []) ])
let c2 = N(false, [])
let h = N(false, [ c1; c2 ])


// 2
let rec descriptionOf =
    function
    | N(x, []) -> [ x ]
    | N(x, cs) -> List.fold (fun acc c -> acc @ (descriptionOf c)) [ x ] cs

// 3

let rec map f =
    function
    | N(x, []) -> N(f x, [])
    | N(x, cs) -> N(f x, List.fold (fun acc c -> acc @ [ (map f c) ]) [] cs)

// 4


let rec numberOf o =
    match o with
    | N((s, i), []) -> i
    | N((s, i), list) -> List.fold (fun acc x -> acc + numberOf x) i list


//
let nat = Seq.initInfinite id

let nat3 =
    seq {
        for i in nat do
            yield (i * 3, i * 3 + 1, i * 3 + 2)
    }

nat3

let nat3 = Seq.map (fun i -> (i * 3, i * 3 + 1, i * 3 + 2)) nat
nat3

let flip sq = Seq.map (fun (x, y) -> (y, x)) sq
