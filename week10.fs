// Problem 1 exam set may 2021

let rec ins x =
    function
    | (y, n) :: ys when x = y -> (y, n + 1) :: ys
    | pair :: ys -> pair :: ins x ys
    | [] -> [ (x, 1) ]

// Give an argument of ins is 'a -> ('a * int) list -> ('a * int) list when 'a: equality
// The function takes 2 input, x and a list of pairs. we see this is in the match cases, as we're matching on list of pairs in the first arm
// The type x and the first element of the pair must have the same type as they
// compared in the first match arm. The there are no operations that require them to have a specific type,
// so we x: 'a. The 2nd element of the pair has addition with 1, so it must be of type int.
// so x: 'a -> arg1: (a' * int) list is the input.
// The output is also seen as a (a' * int) list from the match branches, and then we need to add the equality,
// as we have an equality check in the first match arm between x and y, which are both of type 'a
// therefore we get the type x: 'a -> arg1: (a' * int) list -> (a' * int) list when 'a: equality

let rec cntBy f xs acc =
    match xs with
    | [] -> acc
    | x :: rest -> cntBy f rest (ins (f x) acc)

// (’a -> ’b) -> ’a list -> (’b * int) list -> (’b * int) list when ’b : equality
// First we look at the input of the function
// We see that the first input is a function, the second input is a list. The third input must be a list of pairs with
// some generic type, as the first element, and int as the second element, as it is the input and output of the ins function.
// we give the function the type ('a -> 'b) as we do not know the type of input the function takes, and the type of output
// which depends on the function that is given as input
// xs must then be an 'a list, as individual elements of the list can be used in the function f
// The input of cntBy must then be f: ('a -> 'b)  xs: 'a list     acc: ('b * int) list
// The output of cntBy is then ('b * int) list as the output is acc, and we also have the equality from ins,
// which is equality check on 'b as it takes the output from f
// so we get the function type:
// f: ('a -> 'b)  -> xs: a' list  -> acc: ('b * int) list -> ('b * int) list when 'b : equality
// These are the most generic types for both, as the only type that cant be changed is the int, as that is specified in the ins function
// if you input a list of type int as xs, then a' is type int, which is less general than just type 'a

let rec ins x =
    function
    | (y, n) :: ys when x = y -> (y, n + 1) :: ys
    | pair :: ys -> pair :: ins x ys
    | [] -> [ (x, 1) ]

let rec cntBy f xs acc =
    match xs with
    | [] -> acc
    | x :: rest -> cntBy f rest (ins (f x) acc)

// countBy (fun x -> x%2) [1 .. 3] = cntBy  (fun x -> x%2) [1 .. 3] []
// = cntBy (fun x -> x%2) [2;3] (ins 1%2 []) = cntBy (fun x -> x%2) [2;3] [(1,1)]
// = cntBy (fun x -> x%2) [3] (ins 2%2 [(1,1)]) = cntBy (fun x -> x%2) [3] (ins 0 [(1,1)])
// (ins 0 [(1,1)]) = [(1,1)]::(ins 0 []) = [(1,1);(0,1)]
// cntBy (fun x -> x%2) [3] [(1,1);(0,1)] = cntBy(fun x -> x%2) [] (ins (3%2) [(1,1);(0,1)])
// (ins (3%2) [(1,1);(0,1)]) = (1,2)::[(0,1)]
// cntBy (fun x -> x%2) [] [(1,2);(0,1)] = [(1,2);(0,1)]



// problem 2 exam set december 2021

type T =
    | One of int
    | Two of int * T * int * T

let rec f p t =
    match t with
    | One v when p v -> [ v ]
    | Two(v1, t1, _, _) when p v1 -> v1 :: f p t1
    | Two(_, _, v2, t2) -> v2 :: f p t2
    | _ -> []

// Give the type of f and describe what f computes
// First of all we see that f takes 2 input, p, which is a function that returns a bool, and takes an integer as input.
// and then t, which is of type T, because of the match arms.
// It then returns a list of integers, as we see that v, v1 and v2 are int, and the first match branch returns a list of v.
// the type of f is then:
// f: p: (int -> bool) -> t: T -> int list
// The type T can be seen as a tree structure. f computes a list of integers from a T. T can either be a single int
// or an 2 ints and 2 T, and they are are used as pairs in this function as seen in the match arms (eg. Two(v1,t1,v2,t2)).
// If v1 holds for some p, then we add v1 to the list, and take the path, t1, that is associated with v1.
// else we add v2 to the list, and take the path t2, associated with v2. when we reach a leaf, we add this leaf to the list,
// if it holds for some p, otherwise return the empty list
// Basically, f computes a path from the root of the tree to the leaf, but only adds the leaf if it also holds for p

// Test descriptions
//      Notice that the declaration of f has a match expression with 4 clauses marked C1 to C4 in
//      comments.
//      A test description for f consists of
//       a value pv for argument p,
//       a value tv for argument t,
//       the expected value of f pv tv, and
//       an enumeration of the clauses that are selected during evaluation of f pv tv. The
//         order in which clauses are enumerated is not significant. Repeated enumeration of a
//         clause is not necessary.
//
// description 1:
// p_v = (fun x -> x>2)
// t_v = One(1)
// f p_v t_v = []
// Clauses =
//      | (* C4 *)

// desctription 2:
// p_v = (fun x -> x>2)
// t_v = Two(3, Two( 1,One(1),1,One(5) ), 1, One(5))
// f p_v t_v = [3;1;5]
// Clauses =
//      | (* C1 *)
//      | (* C2 *)
//      | (* C3 *)




// Problem 3 exam december 2021

type Trie<'a> = N of 'a * bool * Children<'a>
and Children<'a> = Trie<'a> list

// Declare a function that counts the number of nodes of a trie. For example, t3 has 6 nodes.
let rec countNu (t: Trie<'a>) : int =
    match t with
    | N(_, _, c) when c = [] -> 1
    | N(_, _, c1) -> List.fold (fun acc x -> acc + (countNu x)) 1 c1


let t1 = N(0, false, [ N(0, false, [ N(1, false, []) ]) ])
let t2 = N(0, true, [ N(0, false, [ N(1, true, []) ]) ])
let ta = N(1, true, [ N(2, true, []) ])
let tb = N(3, false, [ N(0, true, []) ])
let tc = N(2, true, [])
let t3 = N(0, false, [ ta; tb; tc ])


// Declare a function accept w t that can check whether word w is accepted by trie t. Give the type of accept.
// we model the word w as a list, as that is the example they gave in the problem



let rec accept (w: 'a list) (t: Trie<'a>) : bool =
    match (w, t) with
    | ([], _) -> false
    | ([ x ], N(l, true, c)) when x = l -> true
    | (x :: xs, N(l, _, c)) when x = l -> List.fold (fun acc t1 -> acc || (accept xs t1)) false c
    | _ -> false

// type of accept is w: 'a list -> t: Trie<'a> -> bool: when 'a: equality

// Declare a function wordsOf: Trie<’a> -> Set<’a list> that gives the set of words
// accepted by a trie t.
f
let wordsOf (t: Trie<'a>) : Set<'a list> =
    let rec wordsOf' (t: Trie<'a>) (w: 'a list) =
        match t with
        | N(l, true, []) -> Set.add (w @ [ l ]) Set.empty
        | N(l, true, c) ->
            Set.union
                (Set.add (w @ [ l ]) Set.empty)
                (List.fold (fun acc x -> Set.union acc (wordsOf' x (w @ [ l ]))) Set.empty c)
        | N(l, false, c) -> List.fold (fun acc x -> Set.union acc (wordsOf' x (w @ [ l ]))) Set.empty c
        | _ -> Set.empty

    wordsOf' t []

// Declare a function that can check whether a trie contains useless leaves

let rec isValid (t: Trie<'a>) : bool =
    match t with
    | N(_, false, []) -> false
    | N(_, true, []) -> true
    | N(_, _, c) -> List.fold (fun acc x -> acc && isValid x) true c


// Declare a function that computes the degree of a trie.
let rec degree (t: Trie<'a>) : int =
    match t with
    | N(_, _, []) -> 0
    | N(_, _, c) -> List.fold (fun acc x -> max acc (degree x)) (List.length c) c


