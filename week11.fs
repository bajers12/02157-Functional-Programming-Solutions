// week 10 problems from book
// 9.6
// Declare a continuation-based version of the factorial function and compare the run time with
// the results in Section 9.4.

let factC n =
    let rec factC' n c =
        match n with
        | 1 -> c 1
        | _ -> factC' (n - 1) (fun res -> c (res * n))

    factC' n id



let rec factA =
    function
    | (0, m) -> m
    | (n, m) -> factA (n - 1, n * m)

factA (1000000000, 1)

factC 1000000000

let rec fac =
    function
    | 0 -> 1
    | n -> n * fac (n - 1)

fac 1000000000



let rec fib =
    function
    | 0 -> 1
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

let fibA n =
    let rec fibA' n f1 f2 =
        match n with
        | 0 -> f2
        | _ -> fibA' (n - 1) (f2) (f2 + f1)

    fibA' n 0 1


let rec fibC n c =
    let rec fibC' n f1 f2 c =
        match n with
        | 0 -> c 0
        | _ -> fibC' (n - 1) (f2) (f1 + f2) (fun res -> res + (f1 + f2))

    fibC' n 0 1 c

printfn "%d" 10



// exam summer 2014 problem 1

// Evaluates to n^k
// f 10 3 = 10*(f 10 (3-1)) =10*10*(f 10 (2-1)) = 10*10*10*f(10 (1-1)) = 10*10*10*1
// Types are n: int -> arg1: int -> int

let rec g p f =
    function
    | [] -> []
    | x :: xs when p x -> f x :: g p f xs
    | _ :: xs -> g p f xs
// takes a function p: ('a-> bool), a function f: ('a->'b) and a list: 'a list as input, gives a b' list as output
// for every element of the list, if some predicate p holds for x, then apply a function f to x, and cons that to the result list
// if some predicate p does not hold for x, do not apply f to x, and move on the next element.
// an example usage of the function could be:
// g (fun x -> x <> 0) (fun x -> abs x) [-1;2;0;-3] = [1;2;3]
// where p = (fun x -> x <> 0)
// f = (fun x -> abs x)
// list = [-1;2;0;-3]
// this removes all zero elements of the list, and changes all elements to be the absolute value
// type is p:('a -> bool) -> f:('a -> 'b) -> arg1:'a list -> 'b list

// 3. The function f is not tail recursive.
let rec f n =
    function
    | 0 -> 1
    | k when k > 0 -> n * (f n (k - 1))
    | _ -> failwith "illegal argument"

//  1. Make a tail-recursive variant of f using an accumulating parameter.
let rec fA n k acc =
    match k with
    | 0 -> acc
    | k when k > 0 -> fA n (k - 1) (n * acc)
    | _ -> failwith "illegal argument"

let fB n k =
    let rec fB' n k c =
        match k with
        | 0 -> c 1
        | k when k > 0 -> fB' n (k - 1) (fun res -> c (res * n))
        | _ -> failwith "Illegal argument"

    fB' n k id

#time
fB 1 1000000000
fA 1 1000000000 0
//  2. Make a continuation-based tail-recursive variant of f.


// type T =
//     | A of int
//     | B of string
//     | C of T*T;;

// let rec h = function
//     | A n -> string n
//     | B s -> s
//     | C(t1,t2) -> h t1 + h t2;;

// // Takes a type T, which has a tree structure, and adds all the elements of the leaf together as a string
// // h (C(C(A(1), B("a")), A(2))) = "1a2"
// // computes the string of all leafs concatenated together from left to right
// // function has type arg1: T -> string





// Fall exam 2014 problem 2 (4,5)

// 4
let rec f i =
    function
    | [] -> []
    | x :: xs -> (x + i) :: f (i * i) xs


// takes an i: int as input and list: int list as the second input.
// returns an int list so most general type is i: int -> arg1: int list -> int list
// if we look at this function iteratively it adds i(^(2)^k) to every integer x in the list,
// where k is the amount elements we have iterated over starting at k=0
// an example would be f 2 [1;2;3;4]
// this would result in the input [1+2^1;2+2^2;3+2^4;4+2^8] = [3;6;19;260]


// 5

let f i list =
    let rec f' i list acc =
        match list with
        | [] -> acc
        | x :: xs -> f' (i * i) xs ((x + i) :: acc)

    List.rev (f' i list [])

let f i list =
    let rec f' i list c =
        match list with
        | [] -> c []
        | x :: xs -> f' (i * i) xs (fun res -> c ((x + i) :: res))

    f' i list id



// week 11 file problems



let rec takeWhile p =
    function
    | x :: xs when p x -> x :: takeWhile p xs
    | _ -> []

// 1.   Give an argument showing that (’a -> bool) -> ’a list -> ’a list is the most
//      general type of takeWhile. That is, any other type for takeWhile is an instance of
//      (’a -> bool) -> ’a list -> ’a list.

// we see that the p returns a bool as it is used in the first branch as a bool
// we also see that the 2nd input is a list, as it is being matched on as a list.
// it does not have any specified type as it is not operated on, apart from being passed into the function p
// the input types are therefore p: ('a -> bool)    arg1: 'a list
// the output is a list, as we see in the first branch we cons an integer with a list, and the wildcard returns a list.
// x is being put into the list, and we know that x: 'a
// therefore the function has the type ('a -> bool) -> 'a list -> 'a list.
// 'a can be any type, on the condition that 'a is the same type in throughout the whole function.


let rec skipWhile p =
    function
    | x :: xs when p x -> skipWhile p xs
    | xs -> xs

let diff5 n = n <> 5
// Give an evaluation of the expression skipWhile diff5 [2;6;5;1;5;6].
// Use the notation e1 ⇝ e2 from the textbook and include at least as many steps as there are recursive calls
//    skipWhile diff5 [2;6;5;1;5;6]
// -> skipWhile diff5 [6;5;1;5;6]
// -> skipWhile diff5 [5;1;5;6]
// -> [5;1;5;6]

// Describe what takeWhile and skipWhile compute. Your descriptions should focus on
// what they compute, rather than on individual computation steps.
// takeWhile computes a list of all elements that satisfy the predicate p, until it hits an element in the list
// that does not satisfy p
// meaning when it meets an element that does not satisfy p, it will not consider any elements after that element,
// and return a list of all elements that has been evaluated before the element that did not satisfy p

// skipWhile computes a list of elements from the input list. it recurses through a list,
// until it meets an element that does not satisfy a predicate p.
// it discards all elements that satisfies p, and when it finds an element that does not satisfy p,
// it then returns the rest of the list.

// Consider each of the above declarations and explain briefly whether the considered
// function is tail recursive or not. If you encounter a function that is not tail recursive,
// then provide a declaration of a tail-recursive variant with an accumulating parameter
// for that function.

// skipWhile is already tail recursive, as it accumulates it result by passing the list xs every call
// takeWhile is not tail recursive as it does use an accumulating parameter, or a continuation.
// a tail recursive version of takeWhile could look like this:


let takeWhile p x =
    let rec takeWhile' p x acc =
        match x with
        | x :: xs when p x -> takeWhile' p xs (acc@[x])
        | _ -> acc

    takeWhile' p x []


let takeWhile p x =
    let rec takeWhile' p x acc =
        match x with
        | x :: xs when p x -> takeWhile' p xs (acc@[x])
        | _ 

// where we accumulate each step of the iteration in acc




// problem 2 week 11 file
type T =
    | Leaf of char
    | Branch of T * T

// Make an F# value for the tree t0 shown above and declare a function
//      toList: T -> char list
// that gives the list of characters occurring in a tree. The sequence in which the characters
// occur in the list is of no significance.

// the tree looks like:
// t0 = (Branch(Leaf('a'), Branch(Leaf('b'),Leaf('c'))));;
//
//

let rec toList t =
    match t with
    | Leaf(x) -> [ x ]
    | Branch(b1, b2) -> (toList b1) @ (toList b2)

let t0 = (Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c'))))


let rec isLegalNo t =
    match t with
    | Leaf(x) -> 1
    | Branch(b1, b2) -> (isLegal b1) + (isLegal b2)

let rec count t c =
    match t with
    | Leaf(x) when x = c -> 1
    | Branch(b1, b2) -> (count b1 c) + (count b2 c)
    | _ -> 0

let isLegal t =
    isLegalNo t > 1 && List.forall (fun x -> (count t x) < 2) (toList t)

type Dir =
    | L // go left
    | R // go right

type Code = Dir list
type CodingTable = Map<char, Code>


let rec encode (ct: CodingTable) (t: char list) : Code =
    match t with
    | [] -> []
    | x :: xs ->
        match Map.tryFind x ct with
        | Some(l) -> l @ (encode ct xs)
        | None -> failwith ($"{x} is not in the tree")

let rec path (t: T) (c: char) (p: Code) : Code =
    match t with
    | Leaf(x) when x = c -> p
    | Branch(b1, b2) -> (path b1 c (p @ [ L ])) @ (path b2 c (p @ [ R ]))
    | _ -> []


let ofT (t: T) : CodingTable =
    let chars: char list = toList t
    List.fold (fun acc x -> Map.add x (path t x []) acc) Map.empty chars



let rec firstCharOf (t: T) (c: Code) : char * Code =
    match t, c with
    | (Leaf(x), _) -> (x, c)
    | (_, []) -> (' ', [])
    | (Branch(b1, b2), x :: xs) -> if x = L then firstCharOf b1 xs else firstCharOf b2 xs

let rec decode (t: T) (c: Code) : char list =
    match c with
    | [] -> []
    | x ->
        let (a, list) = firstCharOf t x
        if a = ' ' then [] else a :: (decode t list)



let rec fibA n (f1,f2) =
    match n with
    | 0 -> f2
    | n when n<0 -> failwith "Wrong input"
    | _ -> fibA (n-1) (f2, f1+f2)

let fib n = fibA n (0,1)






let sumListFold xs =
    List.fold (fun acc x -> acc + x) 0 xs

let rec sumListTR xs acc =
    match xs witha
    | [] -> acc
    | x::xtail -> sumListTR xtail (acc + x)



let listFold xs =
    List.fold (fun acc x -> (x+1)::acc) [] xs

let listFoldBack xs = 
    List.foldBack (fun x acc -> (x+1)::acc) xs []

let xs = [1;2;3;4];;
let f = listFold xs;;
let fb = listFoldBack xs;;


let listFoldAccList xs =
    List.foldBack (fun x acc -> (d))