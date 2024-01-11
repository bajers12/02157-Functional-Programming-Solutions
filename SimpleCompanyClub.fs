type no = string
type name = string
type yb = int
type ths = string list
type info = no * yb * ths
type register = (name * info) list
type targetgroup = (name * no) list

let reg =
    [ ("John", ("123", 1981, [ "soccer"; "jazz" ]))
      ("Mary", ("456", 1982, [ "soccer"; "jazz"; "swimming" ]))
      ("Peter", ("789", 1981, [ "swimming" ]))
      ("Paul", ("101", 1982, [ "jazz"; "swimming" ]))
      ("Rosie McGee", ("001", 1998, [ "basketball"; "R&B"; "swimming" ])) ]


let p1 (_, yb, ths) =
    yb > 1981 && List.contains "soccer" ths && List.contains "jazz" ths

let p2 (_, yb, ths) =
    yb > 1981 && (ths |> List.contains "soccer" || ths |> List.contains "jazz")

let getNo (no, _, _) = no

let p3 (_, yb, ths) = yb > 1980

let rec extractTargetGroup p reg =
    match reg with
    | [] -> []
    | (name, info) :: tail when p info -> (name, getNo info) :: extractTargetGroup p tail
    | (name, info) :: tail -> extractTargetGroup p tail



//rosie mcgee
let rec f =
    function
    | (x, []) -> []
    | (x, y :: ys) -> (x + y) :: f (x - 1, ys)

let rec g =
    function
    | [] -> []
    | (x, y) :: s -> (x, y) :: (y, x) :: g s

let rec h =
    function
    | [] -> []
    | x :: xs -> x :: (h xs) @ [ x ]




let rec extractTargetGroup p r =
    match r with
    | [] -> []
    | (name, (no, yb, ths)) :: tail when p (no, yb, ths) -> (name, no) :: extractTargetGroup p tail
    | (name, (no, yb, ths)) :: tail -> extractTargetGroup p tail


let p4 (_, yb, ths) =
    yb > 1981
    && List.exists (fun x -> x = "jazz") ths
    && List.exists (fun x -> x = "swimming") ths
