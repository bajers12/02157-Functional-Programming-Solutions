// problem 1
// Give an example of an application of each of the functions f, g and h.

let rec f n = function | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;

let rec g p f = function
    | [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;

type T =
    | A of int
    | B of string
    | C of T*T;;
let rec h = function
    | A n -> string n
    | B s -> s
    | C(t1,t2) -> h t1 + h t2;;

// application of f:
// f 10 3 = 10^3
// application of g
// g (fun x -> x>0) (fun x -> x*3) [1;2;0;3] = [3;6;9]
// application of h:
// h C(A 3, B "a") = "3a"

// Most general type of f is int -> int -> int
// computes n^k

// most general type of g is ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
// takes a list and creates a new list with all elements that holds for the predicate p
// applies a function f to all those elements, and returns the new list with the function applied to each individual element
// that holds for p

// most general type of h is T -> string
// changes leafs of type A into string, and concatenates all A leafs and B leafs together from left to right


// make f tailrecursive using accumulating parameters

let f n k =
    let rec f' n acc = function
        | 0 -> acc
        | k when k>0 -> f' n (acc*n) (k-1)
        | _ -> failwith "Illegal argument"
    f' n 1 k

let f n k =
    let rec f' n c = function
        | 0 -> c 1
        | k when k>0 -> f' n (fun res -> c res*n) (k-1)
        | _ -> failwith "Illegal argument"
    f' n id k

// types of sq and k
// sq has the type seq<int> and k has the type seq<int*int>
// sq is an infinite sequence, that computes all multiples of 3
// k is and infinite sequence that computes tuples, containing multiples of 3 in the first element,
// and the second element is the first element minus j


// xs returns [0;3;6;9]
// ys return [(0,-2);(3,1);(6,4);(9,7)]

let sq = Seq.initInfinite (fun i -> 3*i);;
let k j = seq { for i in sq do
                yield (i,i-j) };;
let xs = Seq.toList (Seq.take 4 sq);;
let ys = Seq.toList (Seq.take 4 (k 2));;

// problem 2

let rec ordered = function
    | [] -> true
    | [x] -> true
    | x1::x2::xs when x1<x2 -> ordered (x2::xs)
    | _ -> false

// type of ordered is 'a list -> bool

let smallerThan x xs =
    List.forall (fun x' -> x < x') xs

// smallerThan has the type 'a -> 'a list -> bool

let rec insertBefore p x xs =
    match xs with
    | [] -> [x]
    | x'::xs when p x' -> x::x'::xs
    | x'::xs -> x'::insertBefore p x xs




let sexToString s =
    match s with
    | M -> "Male"
    | F -> "Female"

let replicate n s =
    let rec replicate' n s acc = 
        match n with
        | 0 -> acc
        | k when k > 0 -> replicate' (n-1) s (s+acc)
        | _ -> failwith "n must be larger than 0"
    replicate' n s ""
         
// problem 3
type Name = string;;
type Sex =
    | M
    | F
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

let sexToString s =
    match s with
    | M -> "Male"
    | F -> "Female"


let rec isWF (t: FamilyTree) : bool =
    match t with
    | P (_,_,_,[]) -> true
    | P (n,s,yob,cs) -> let rec childAge (c: Children) =
                            match c with
                            | [] -> []
                            | P(_,_,y,_)::xs -> y :: childAge xs
                        let cy = childAge cs
                        (List.forall (fun c -> isWF c) cs) && (yob < List.max cy) && (ordered cy)


let makePerson (n,s,yob) =
    P(n,s,yob,[])

// let rec insertChildOf n c t =
//     match t with
//     | P(n',s,yob,cs) when n' = n -> let t' = insertBefore (fun (_,_,y,_) -> y > yob ) c cs
//                                     if isWF t' then Some(t') else None
//     | P(n',s,yob,cs) -> let cs' = insertChildOfInList n c cs
                        
// let rec insertChildOfInList n c cs = 
    





let rec find' n t =
    match t with
    | P(n', s, yob, []) when not (n = n') -> None
    | P(n', s, yob, cs)  when n = n' -> let rec childNames xs =
                                            match xs with
                                            | [] -> []
                                            | P(n,_,_,_)::ys -> n::childNames ys
                                        Some((n', s, yob, childNames cs))
    | P(_,_,_,cs) ->    let oList = List.fold (fun acc c -> (find' n c)::acc) [] cs
                        let rec findOption xs =
                            match xs with
                            | [] -> None
                            | Some(n',s,yob,cs)::ys -> Some(n',s,yob,cs)
                            | _::ys -> findOption ys
                        findOption oList

let find n t = 
    let res = find' n t
    match res with
    | Some (n, s, yob, cs) -> (n, s, yob, cs)
    | None -> failwith "Name does not exist in family tree"


let toString n t =
    let rec toString' n i t =
        match t with
        | P(name, s', yob, []) ->   let ss = sexToString (s')
                                    let yobs = string yob
                                    (replicate (n*i) " ") + $"{name} {ss} {yobs} \n"
        |P(name, s', yob, cs) ->    let ss = sexToString (s')
                                    let yobs = string yob
                                    let stri = (replicate (n*i) " ") + $"{name} {ss} {yobs} \n"
                                    let stringr = List.fold(fun acc c -> acc + (toString' n (i+1) c)) "" cs
                                    stri + stringr
                                    
    toString' n 0 t


let rec truncate t = 
    match t with
    | P(n,s,yob,cs) ->  match s with
                        | M -> P(n,s,yob, (List.foldBack(fun c acc -> (truncate c)::acc) cs []))
                        | F -> P(n,s,yob,[])


let pet = P("Peter",M,2005,[])
let bob = P("Bob",M,2008,[])
let eve = P("Eve",F,2010,[])
let fred = P("Fred", M, 1970, [])
let joan = P("Joan", F, 1975, [])
let stan = P("Stanley", M, 1975, [])
let mary = P("Mary", F, 1980, [pet;bob;eve])
let jane = P("Jane",F,1985,[])
let may = P("May", F, 1945, [fred;joan])
let joe = P("Joe", M, 1950, [stan;mary;jane])
let paul = P("Paul",M,1955,[])
let larry = P("Larry", M, 1920, [may;joe;paul])

isWF larry;;
find "Jane" larry;;
find "Mary" larry;;
toString 6 larry;;
toString 6 (truncate larry);;