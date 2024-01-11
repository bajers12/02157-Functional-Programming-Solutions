// problem 1
// 1
type Appliance = string
type Usage = Appliance * int
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2; ("woman",2)]




let rec inv = function
    | [] -> true
    | (_,x)::xs when x > 0 -> inv xs
    | (_,x)::xs -> false;;

let rec durationOf ap = function
    | [] -> 0
    | (x,y)::xs when ap = x -> y + durationOf ap xs
    | (_,_)::xs -> durationOf ap xs;;



let wellFormed ats =
    (inv ats) && (List.forall (fun (x,y) -> (durationOf x ats) <= 24) ats);;


let rec delete a = function
    | [] -> []
    | (x,_)::xs when a = x -> delete a xs
    | (x,y)::xs -> (x,y)::delete a xs;;


type Price = int
type Tariff = Map<Appliance, Price>

let isDefined ats trf =
    List.forall (fun (x,_) ->   match Map.tryFind x trf with
                                | None -> false
                                | _ -> true) ats;;

let priceOf ats trf =
    List.fold (fun acc (x,y) -> match Map.tryFind x trf with
                                | None -> failwith $"{x} is not in the tariff"
                                | Some(p) -> acc + (p*y) ) 0 ats;;

let trf = Map.ofList [("washing machine",5); ("dishwasher",3); ("coffee machine",2)]
priceOf ats trf;;
// problem 2

let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

// g1 has the type ('a -> bool) -> 'a list -> 'a list
// It takes a list and keeps all elements of the list, until it reaches an element which does not satisfy p
// it then returns that list

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;

// g2 has the type ('a -> 'a) -> ('a -> 'a) -> int -> 'a -> 'a
// it takes an input x, then applies a function f to it, but in the recursive call, f and h are switched, meaning h is applied in the next recursing
// it applies f on x, then h on the result on that, then f on the result of that, until x = 0


// 2
let rec g1Acc' p acc = function
    | x::xs when p x -> g1Acc' p (acc@[x]) xs
    | _ -> acc;;

let g1Acc p xs = g1Acc' p [] xs

let rec g1C' p c = function
    | x::xs when p x -> g1C' p (fun res -> c(x::res)) xs
    | _ -> c []

let g1C p xs = g1C' p id xs;;

// 3
// g2 is tail recursive, using an accumulating parameter, as it accumulates the value and passes that on with each recursing
// and in the end returns the accumulated value when n = 0

// 4 
// value of List.ofSeq (f1 2 2 3) is:
// [(0,0);(0,1);(0,2);(1,0);(1,1);(2,0)]
let f1 m n k = seq {    for x in [0..m] do
                            for y in [0..n] do
                                if x+y < k then
                                     yield (x,y) };;

let f2 p f sq =
    Seq.filter p sq |> Seq.map f 

// f1 has the type int -> int -> int -> Seq<int*int>
// it computes a sequence of all pairs, that when added together are less than k

// f2 has the type ('a -> bool) -> ('a-> 'b) -> Seq<'a> -> Seq<'b>
// it takes a sequence, and applies a filter on it, removing all elements that do not satisfy p

// f3 has the type (a' -> 'b) -> Seq<'a> -> Seq<'b>
// it applies a function to every element of the sequence, can also be replaced by Seq.map


// problem 3
type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list

// 1

let riv1 = R("R1", 5 , [])
let riv4 = R("R4", 2, [])
let riv2 = R("R2", 15, [riv4] )
let riv3 = R("R3", 8, [])

let riv = R("R",10, [riv1;riv2;riv3] );;


let rec contains n r =
    match r with
    | R(n',_,_) when n = n' -> true
    | R(_,_,[]) -> false
    | R(_,_,r') -> List.exists(fun x -> contains n x) r';;

contains "R5" riv;;


let rec allNames = function
    | R(n,_,[]) -> [n]
    | R(n,_,r) -> List.fold (fun acc x -> (allNames x)@acc) [] r;;
    
let rec totalFlow = function
    | R(_,fl, []) -> fl
    | R(_,fl, r) -> List.fold(fun acc x -> (totalFlow x) + acc) fl r;;


let rec allSources = function
    | R(n,fl,[]) -> [(n,fl)]
    | R(n,fl,r) -> List.fold( fun acc x -> acc@ (allSources x )) [(n,fl)] r;;

let rec maxSource (r,fl) = function
    | [] -> (r,fl)
    | (r',fl')::xs when fl' > fl -> maxSource (r',fl') xs
    | (_,_)::xs -> maxSource (r,fl) xs ;;

let mainSource r =
    allSources r |> maxSource ("",0);;


mainSource riv;;
mainSource riv3;;

let tryInsert n t r =
    let rec tryInsert' n t = function
        | R(n',fl,r') when n = n' -> R(n',fl,r'@[t])
        | R(n',fl,r') -> R(n',fl,List.fold(fun acc x -> (tryInsert' n t x)::acc) [] r')
        
    if contains n r then Some(tryInsert' n t r) else None

tryInsert "R4" (R("R5",7,[])) riv;;






let nat = Seq.initInfinite id;;

let nat3 = seq {for i in nat do
                    yield (i*3,i*3+1,i*3+2)}

nat3;;