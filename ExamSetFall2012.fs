// Problem 1

type Name = string
type Score = int
type Result = Name * Score

let legalResults xs =
    List.forall (fun x -> x >= 0 && x <= 100) xs

let maxScore xs =
    List.fold (fun acc (_, x) -> max acc x) 0 xs

let best xs =
    List.fold (fun (n, s) (n', s') -> if s' > s then (n', s') else (n, s)) ("", 0) xs

let average xs =
    let (sum, ns) = List.fold (fun (s, n) (_, s') -> (s + s', n + 1)) (0, 0) xs
    (float sum) / (float ns)

let rec delete r xs =
    match xs with
    | [] -> []
    | x :: tail when x = r -> tail
    | x :: tail -> x :: delete r tail


let rec bestN xs n =
    match n with
    | 0 -> []
    | _ ->
        let r = best xs
        r :: bestN (delete r xs) (n - 1)




// Problem 2

type Typ =
    | Integer
    | Boolean
    | Ft of Typ list * Typ

type Decl = string * Typ


let rec varsList xs =
    match xs with
    | [] -> []
    | (x, _) :: tail -> x :: varsList tail

let rec distinctVars (xs: Decl list) =
    match xs with
    | [] -> true
    | (x, _) :: tail when List.contains x (varsList tail) -> false
    | (_, _) :: tail -> distinctVars tail


type SymbolTable = Map<string, Typ>

let toSymbolTable (xs: Decl list) =
    List.fold (fun acc (d, t) -> Map.add d t acc) Map.empty xs




let rec extendST (sym: SymbolTable) (xs: Decl list) =
    match xs with
    | [] -> sym
    | (d, t) :: tail -> extendST (Map.add d t sym) tail

type Exp =
    | V of string
    | A of string * Exp list

let rec symbolsDefined sym e =
    match e with
    | V(x) -> Map.containsKey x sym
    | A(_, xs) -> List.forall (fun x -> symbolsDefined sym x) xs


let isBool x = true

let isAOp x = true

let rec typOf sym =
    function
    | V(x) -> Map.find x sym
    | A(x, el) ->
        match x with
        | x when isBool x -> Ft(genTypList sym el, Boolean)
        | x when isAOp x -> Ft(genTypList sym el, Integer)
        | _ -> failwith "Expression is not well typed"

and genTypList sym el =
    List.foldBack (fun e acc -> (typOf sym e) :: acc) el []


// problem 3

// I wrote:
// Type of h is:
// a:'a list -> b: 'a list -> 'a list
// the function appends the list b at the end of list a
// WRONG, it takes 2 lists and merges them together.
// but type was correct, i saw that right after putting the function in T_T
let rec h a b =
    match a with
    | [] -> b
    | c :: d -> c :: (h b d)

type T<'a, 'b> =
    | A of 'a
    | B of 'b
    | C of T<'a, 'b> * T<'a, 'b>

C(A(3), B(true))

C(A([ 1; 2 ]), B(Some("a")))

let rec f1 =
    function
    | C(t1, t2) -> 1 + max (f1 t1) (f1 t2)
    | _ -> 1
// type is T<'a,'b> -> int
// computes the maximum depth of a tree

let rec f2 =
    function
    | A e
    | B e -> [ e ]
    | C(t1, t2) -> f2 t1 @ f2 t2
// Type is T<'a,'a> -> 'a list
// returns a list of all elements in the leafs of the tree

let rec f3 e b t =
    match t with
    | C(t1, t2) when b -> C(f3 e b t1, t2)
    | C(t1, t2) -> C(t1, f3 e b t2)
    | _ when b -> C(A e, t)
    | _ -> C(t, B e)

// type is 'a -> bool -> T<'a,'a> -> T<'a,'a>
// If b is true then recurse to the left most leaf in the tree, change that to a branch, where the leaf from before
// is the right leaf, and A e is the left leaf
// opposite if b is false, then go to the right leaf, and change it to a branch
// make the left leaf the original leaf, and make the right leaf B e
