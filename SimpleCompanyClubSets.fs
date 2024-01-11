type no = string
type yb = int
type ths = string list
type name = string
type info = no * yb * ths
type fullInfo = name * info
type register = Set<name * info>
type target = (name * no) list

let reg =
    Set.ofList
        [ ("John", ("123", 1981, [ "soccer"; "jazz" ]))
          ("Mary", ("456", 1982, [ "soccer"; "jazz"; "swimming" ]))
          ("Peter", ("789", 1981, [ "swimming" ]))
          ("Paul", ("101", 1982, [ "jazz"; "swimming" ]))
          ("Rosie McGee", ("001", 1998, [ "basketball"; "R&B"; "swimming" ])) ]


let p1 (_, yb, ths) =
    yb > 1981 && List.contains "soccer" ths && List.contains "jazz" ths

let p2 (_, yb, ths) =
    yb > 1981 && (ths |> List.contains "soccer" || ths |> List.contains "jazz")


let extractTargetGroup p (r: register) : target =
    Set.foldBack
        (fun x acc ->
            let (name, (no, yb, ths)) = x
            if p (no, yb, ths) then (name, no) :: acc else acc)
        r
        []
