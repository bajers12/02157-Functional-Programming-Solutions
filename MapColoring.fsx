// Programs to color a map      --- Michael R. Hansen 27-09-2023
// See Section 5.2 in the textbook and slides from Lectures 4 and 5.

type Map<'c> = ('c * 'c) list
type Color<'c> = 'c list
type Coloring<'c> = Color<'c> list

let exMap = [ ("a", "b"); ("c", "d"); ("d", "a") ]


// areNb: Map<'c> -> 'c -> 'c -> bool when 'c: equality
let areNb m c1 c2 =
    List.contains (c1, c2) m || List.contains (c2, c1) m

// canBeExtBy: Map<'c> -> Color<'c> -> 'c -> bool when 'c: equality
let rec canBeExtBy m col c =
    match col with
    | [] -> true
    | c' :: col' -> not (areNb m c' c) && canBeExtBy m col' c




// extColoring: Map<'c> -> Coloring<'c> -> 'c -> Coloring<'c> when 'c: equality
let rec extColoring m cols c =
    match cols with
    | [] -> [ [ c ] ]
    | col :: cols' ->
        if canBeExtBy m col c then
            (c :: col) :: cols'
        else
            col :: extColoring m cols' c

let addElem x ys =
    if List.contains x ys then ys else x :: ys

// countries: Map<'c> -> 'c list
let rec countries =
    function
    | [] -> []
    | (c1, c2) :: m -> addElem c1 (addElem c2 (countries m))

// colCntrs: Map<'c> -> 'c list -> Coloring<'c>
let rec colCntrs m =
    function
    | [] -> []
    | c :: cs -> extColoring m (colCntrs m cs) c

// colMap: Map<'c> -> Coloring<'c> when 'c: equality
let colMap m = colCntrs m (countries m)

colMap exMap

type Country =
    | A
    | B
    | C
    | D
    | E
    | F

type SmallMap = Map<Country>

#r "nuget: FsCheck"
open FsCheck

// A function checking that all contries in m are in countries m
// prop1: SmallMap -> bool
let prop1 (m: SmallMap) =
    let cs = countries m
    List.forall (fun (c1, c2) -> List.contains c1 cs && List.contains c2 cs) m

// properties validated using FsCheck should be monomorphic
let _ = Check.Verbose prop1
