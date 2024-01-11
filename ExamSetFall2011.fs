// Problem 1
// 1.   Delare a value of type register, that ontains four members: Joe (having phone
//      number: 10101010 and level: 4) , Sal (having phone number: 11111111 and level: 2),
//      Sam (having phone number: 12121212 and level: 7), Jane (having phone number:
//      13131313 and level: 1).

type name = string;;
type phone = int;;
type level = int;;
type desription = phone * level;;
type register = (name * desription) list;;


let reg: register = [("Joe", (10101010, 4)); 
                     ("Sal", (11111111, 2)); 
                     ("Sam", (12121212, 7)); 
                     ("Jane", (13131313,1))];;

// 2.   Delare a funtion getPhone: name -> register -> phone to extrat the phone
//      number of a member in a register. The funtion should raise an exeption Register
//      if the member is not ourring in the register.

let rec getPhone (n: name) (reg: register) :phone =
    match reg with
    | [] -> failwith "Name is not found in register"
    | (name,(p,_))::xs when n = name -> p
    | (name,_)::xs -> getPhone n xs;;


getPhone "Joe" reg;;

let rec delete ((n: name), (reg: register)): register =
    match reg with
    | [] -> []
    | (name,_)::xs when n = name -> xs
    | x::xs -> x::delete (n,xs);;


let getCandidates (l: level) (reg: register): (name*phone) list =
    List.foldBack (fun (n,(nu,l')) acc ->   if abs (l - l') < 3 then (n,nu)::acc
                                            else acc) reg [];;

getCandidates 5 reg;;




// problem 2

type exp =
    | C of int
    | BinOp of exp * string * exp;;

// 3
let exp1 = C(3);;
// 3+5
let exp2 = BinOp(C(3),"+",C(5));;
// (3+5)*10
let exp3 = BinOp(BinOp(C(3),"+",C(5)),"*",C(10));;


let rec toString = function
    | C(x) -> string x
    | BinOp(e1, b, e2) -> "(" + (toString e1) + b + (toString e2) + ")";;


let rec extractOp = function
    | C(x) -> Set.empty
    | BinOp(e1, b, e2) -> Set.union (set [b]) (Set.union (extractOp e1) (extractOp e2));;


type exp = 
    | C of int
    | BinOp of exp * string * exp
    | Id of string
    | Def of string * exp * exp;;


let isDef (e: exp) :bool =
    let rec isDef' (e: exp) (l: string list) =
        match e with
        | C(_) -> true
        | BinOp(e1,_,e2) -> (isDef' e1 l) && (isDef' e2 l)
        | Id(x) ->  if List.contains x l then true
                    else false
        | Def(x, e1, e2) -> let l' = x::l
                            (isDef' e1 l') && (isDef' e2 l')
    isDef' e [];;


// problem 3
type 'a tree = 
    | Lf
    | Br of 'a * 'a tree * 'a tree;;

let rec f(n,t) =
    match t with
    | Lf -> Lf
    | Br(a, t1, t2) ->  if n>0 then Br(a, f(n-1, t1), f(n-1, t2))
                        else Lf;;

// f: (int*'a tree) -> 'a tree
// takes a tree, and then recreates the tree until depth n, 
// where it cuts the rest of the tree off and adds a leaf instead of branches

let rec g p = function
    | Br(a, t1, t2) when p a -> Br(a, g p t1, g p t2)
    | _ -> Lf;;
// ('a -> bool) -> 'a tree -> 'a tree
// takes a predicate p and a tree, and generates the tree
// where every branch holds for the predicate p
// example if p x = x>3
// it then generates the tree with all branches where a>3
// if p is not true, then replace the branch with a leaf


let rec h k = function
    | Lf -> Lf
    | Br(a, t1, t2) -> Br(k a, h k t1, h k t2);;

// ('a -> 'b) -> 'a tree -> 'b tree
// takes a function k, and applies it to a, at all levels of the tree.
// example k x = string x
// would transform the integers of the tree (called a in the Br(a, t1 ,t2) match arm) into a string



// we use an example to prove this


// 