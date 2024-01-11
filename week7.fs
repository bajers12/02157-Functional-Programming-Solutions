// 2013 fall exam problem 1

type Multiset<'a when 'a: equality> = ('a * int) list

let inv (m:Multiset<'a>) =
    let rec inv' m x =
        match m with
        | [] -> 0
        | (a, _)::xs -> if x = a then 1 + inv' xs x else inv' xs x
    
    List.forall(fun (c1,_) -> (inv' m c1) = 1) m

let m = [("b",3); ("a",5); ("d",1)]
let m2 = [("a",3); ("b",5); ("c",1)]

let rec insert (x: 'a) (n: int) (m: Multiset<'a>) =
    match m with
    | [] -> [(x, n)]
    | (a,b)::xs when x = a ->  (a, b+n) :: xs 
    | (a,b)::xs -> (a, b) :: (insert x n xs)

// type of this should be 'a -> (Multiset) -> int
let rec numberOf (e: 'a) (ms:Multiset<'a>) =
    match ms with
    | [] -> 0
    | (a,b)::tail when e=a -> b
    | (a,b)::tail -> numberOf e tail


let rec delete (e: 'a) (ms: Multiset<'a>) =
    match ms with
    | [] -> []
    | (a,b)::tail when e = a -> if b-1 <= 0 then tail else (a,b-1)::tail
    | (a,b)::tail -> (a,b)::(delete e tail)

let rec union ((m1:Multiset<'a>),(m2:Multiset<'a>)) =
    match m1 with
    | [] -> m2
    | (a,b)::tail -> union (tail,(insert a b m2))

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;


let m:MultisetMap<'a> = Map.ofList [("b",3); ("a",5); ("d",1)]


let invMap (m:MultisetMap<'a>)=
    Map.forall (fun _ n -> n > 0) m

let insertMap (x: 'a) (n: int) (m: MultisetMap<'a>) :MultisetMap<'a> =
    Map.add x n m

let unionMap ((m1:MultisetMap<'a>),(m2:MultisetMap<'a>)) :MultisetMap<'a> = 
    Map.fold (fun m a n -> match Map.tryFind a m with
                           | None -> Map.add a n m
                           | Some(e) -> Map.add a (n+e) m) m1 m2
let m1:MultisetMap<'a> = Map.ofList [("b",3);("a",5);("d",1)]
let m2:MultisetMap<'a> = Map.ofList [("a", 3); ("b",4); ("c",2)]

// 2015 fall exam problem 1

type Appliance = string
type Usage = Appliance * int
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

let inv (list: Usage list) =
    List.forall(fun (c1,c2) -> c2>0) list

let durationOf (a: Appliance) (list: Usage list) =
    List.fold(fun acc (c1,c2) -> if a=c1 then acc + c2 else acc) 0 list

let allAppl (list: Usage list) =
    let rec allAppl' (list: Usage list) (newL: Appliance list) =
        match list with
        | [] -> []
        | (a,b)::xs -> if List.contains a newL then allAppl' xs newL else allAppl' xs (newL @ [a])
    allAppl' list []

let rec sumTime (ad:Appliance) (list: Usage List) =


let wellFormed (list: Usage list) =
    let ads = allAppl list
    inv list && List.forall(fun ad -> durationOf ad list <= 24) ads

let delete ((a: Appliance),(ats: Usage list)) =
    List.filter(fun (x,y) -> not(a = x)) ats

type Price = int
type Tariff = Map<Appliance, Price>
let trf = Map.ofList [("washing machine", 7); ("coffee machine", 2); ("dishwasher", 5)]
let isDefined (ats: Usage list) (trf: Tariff) :bool =
    List.forall (fun (a,_) -> match Map.tryFind a trf with
                                | None -> false
                                | Some(_) -> true) ats

let priceOf (ats: Usage list) (trf: Tariff) :int =
    if isDefined ats trf
    then List.fold(fun acc (a,n)  -> Map.find a trf * n + acc) 0 ats
    else failwith("An appliance used is not defined in the tariff")
