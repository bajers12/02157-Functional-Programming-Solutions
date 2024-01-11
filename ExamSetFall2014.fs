type Rel<'a,'b> = ('a * 'b list) list;;


let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;


let rec apply x rel = 
    match rel with
    | [] -> []
    | (x',y)::xs when x = x' -> y
    | (_,_)::xs -> apply x xs;;


let rec inRelation x y rel =
    match rel with
    | [] -> false
    | (x', y')::xs when x=x' -> if List.contains y y' then true else false
    | (_,_)::xs -> inRelation x y xs;; 

inRelation 4 "e" rel;;


let rec insert x y rel= 
    match rel with
    | [] -> [(x,[y])]
    | (x',y'::ys)::xs when x=x' -> (x,y'::ys@[y])::xs
    | (x',y')::xs -> (x',y')::insert x y xs;;

let toRel r =
    let rec toRel' r rel =
        match r with
        | [] -> rel
        | (x,y)::xs -> toRel' xs (insert x y rel)
    toRel' r [];;

toRel [(2,"c");(1,"a");(2,"b")]


// problem 2
let multTable i = Seq.take 10 (Seq.initInfinite (fun i -> (i+1)*3))

let tableOf m n f = seq {for i in {1..m} do
                            for j in {1..n} do
                                yield (i,j, f i j)}
tableOf 3 4 (+);;

let rec repeatString x n =
    match n with
    | 0 -> ""
    | k when k>0 -> x + repeatString x (n-1)
    | _ -> failwith "n must be positive";;


let rep x = Seq.initInfinite (fun i -> repeatString x (i+1));;


let rec f i = function
    | [] -> []
    | x::xs -> (x+i)::f (i*i) xs;;

// general type of f is int -> int list -> int list

// It adds i to an element x which is an element in a list.
// after each recursion i is put to the power of 2,
// first call would be x+i, 2nd call would be x+i^2 3rd call would be x+i^4 and so on

let fAcc i list =
    let rec fAcc' i list acc =
        match list with
        | [] -> acc
        | x::xs -> fAcc' (i*i) xs ((x+i)::acc)
    List.rev (fAcc' i list [])

let fC i list =
    let rec fC' i list c =
        match list with
        | [] -> c []
        | x::xs -> fC' (i*i) xs (fun res -> (x+i)::res)
    fC' i list id


// problem 3


type T<'a> = N of 'a * T<'a> list
let rec f(N(e,es)) = e :: g es
and g = function
    | [] -> []
    | e::es -> f e @ g es;;

let rec h p t =
    match t with
    | N(e,_) when p e -> N(e,[])
    | N(e,es) -> N(e, List.map (h p) es);;
let rec k (N(_, es)) = 1 + List.fold max 0 (List.map k es);;


// give 3 values of type T<string>
N("a",[])
N("a",[N("b",[]);N("c",[])])
N("a",[N("b",[N("d",[])]);N("c",[])])

// Give the (most general) types of f, g, h and k and describe what each of these four functions computes. 
// Your description for each function should focus on what it computes,
// rather than on individual computation steps.

// f is T<'a> -> 'a list
// g is T<'a> list -> 'a list
// h is ('a -> bool) -> T<'a> -> T<'a>
// k is T<'a> -> int
// f and g together computes a list of all values in each nodes of the tree
// h modifies a the tree, if the node value fulfills a predicate then you remove all its children/branches
// else you keep the branches
// k finds the max depth of a tree


// problem 4
type Outcome = | S | F // S: for success and F: for failure
type Sample = Outcome list
type ProbTree = 
    | Branch of string * float * ProbTree * ProbTree
    | Leaf of string

type Description = (Outcome * string) * float * string;;

let rec probOK (t: ProbTree) =
    match t with
    | Leaf(_) -> true
    | Branch(_,p,t1,t2) when 0.0<=p && p <= 1.0 -> probOK t1 && probOK t2
    | _ -> false


let exp = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B")
, Branch(">3",0.5, Leaf "C", Leaf "D"))

probOK exp;;

let rec isSample (os,t) =
    match os,t with
    | ([],Leaf(_)) -> true
    | (x::xs, Leaf(_)) -> false
    | (x::xs, Branch(_,_,b1,b2)) -> if x = S then isSample(xs,b1) else isSample (xs, b2);;
// type of isSample is (Outcome list * Probtree) -> bool

isSample([F;S],exp);;

let descriptionOf (os, t) =
    let rec descriptionOf' os t c p =
        match os, t with
        | ([],Leaf(x)) -> (c,p,x)
        | (x::xs, Branch(d,p',b1,b2)) ->    if x = S then descriptionOf' xs b1 (c@[(x,d)]) (p*p')
                                            else descriptionOf' xs b2 (c@[(x,d)]) (p*(1.0-p'))
        | (_,_) -> failwith "List of outcomes does not fit the tree"
    descriptionOf' os t [] 1;;


descriptionOf([F;S],exp);;




let allDescriptions t =
    let rec allDescriptions' t t' os =
        match t with
        | Leaf(_) -> set [descriptionOf (os, t')]
        | Branch(_,_,b1,b2) -> Set.union (allDescriptions' b1 t' (os@[S])) (allDescriptions' b2 t' (os@[F]))
    allDescriptions' t t [];;

allDescriptions exp;;




let probabilityOf t p =
    let allD = Set.toList (allDescriptions t)
    let rec probabilityOf' d p prob =
        match d with
        |[] -> prob
        | (_,prob', x)::xs when p x -> probabilityOf' xs p (prob+prob')
        | (_,_,_)::xs -> probabilityOf' xs p prob
    probabilityOf' allD p 0.0;;



let p x =
    match x with
    | x' when x' = "C" || x' = "B" -> true
    | _ -> false

// idk about the last question, use the predicate and that will compute the values for all leafs that satisfy the predicate
