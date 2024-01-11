// problem 1

let rec getEs = function
    | [] -> []
    | (a,n)::xs -> a::getEs xs;;

let inv ms =
    let rec inv' xs ms =
        match ms with
        | [] -> true
        | (e,n)::tail when List.contains e xs -> false
        | (e,n)::tail -> inv' xs tail
    inv' (getEs ms) ms;;


let rec insert e n = function
    | [] -> [(e,n)]
    | (e',n')::tail when e = e' -> (e,n+n')::tail
    | (e',n')::tail -> (e',n')::(insert e n tail)

let rec numberOf e = function
    | [] -> 0
    | (e', n)::tail when e = e' -> n
    | (e', n)::tail -> numberOf e tail;;
// the type of numberOf is 'a -> Multiset<'a> -> int


let rec delete e = function
    | [] -> []
    | (e', n)::tail when e=e' -> (e', n-1)::tail
    | (e', n)::tail -> (e', n)::delete e tail;; 


let rec union ms1 ms2 =
    match ms2 with
    | [] -> []
    | (e,n)::tail -> (insert e n)::union ms1 tail;;



let invMap ms =
    Map.forall(fun _ x -> x>0) ms

let insertMap e n ms =
    let n' = Map.find e ms
    Map.add e (n+n') ms

let unionMap ms1 ms2 =
    Map.fold(fun acc e n -> insertMap e n acc) ms2 ms2





// problem 2

let rec f i = function
    | [] -> []
    | x::xs -> (i,x)::f (i*i) xs

// type is int -> 'a list -> (int * 'a) list
// creates a list of pairs, and i is increased everytime, starting at (i^2)^0 at the start, ending at (i^2)^n where n is the length of the list -1


type 'a Tree =
    | Lf
    | Br of 'a Tree * 'a * 'a Tree 

let rec g p = function
    | Lf -> None
    | Br (_,a,t) when p a -> Some t
    | Br(t1,a,t2) ->    match g p t1 with
                        | None -> g p t2
                        | res -> res

// type of this function is ('a -> bool) -> 'a Tree -> 'a Tree option
// traverses the tree in pre order, and finds the first node where a holds for some predicate,
// and return the option of the right branch of that node



let rec fAcc' i xs acc =
    match xs with
    | [] -> acc
    | x::tail -> fAcc' (i*i) (tail) (x::acc)

let fAcc i xs = List.rev(fAcc' i xs [])


let fC i xs =
    let rec fC' i xs c =
        match xs with
        | [] -> c []
        | x::tail -> fC' (i*i) tail (fun res -> c((i,x)::res))
    fC' i xs id


// I definitely prefer the continuation based tail recursive function in this case, as it does not require me to reverse the list
// to keep the original form of the list. Also, the continuation based function might be slower, but it can handle bigger lists.


let A = Seq.initInfinite id;;
Seq.take 4 A;;
let B = seq { for i in A do
                for j in seq {0 .. i} do
                    yield (i,j) };;
Seq.take 2 B;;


let rec h f (n,e) = 
    match n with
    | 0 -> e
    | _ -> h f (n-1, f n e);;
// value of h ( * ) (4,1) would be 1
// the type of h is (int -> 'a -> 'a) -> (int * 'a) -> 'a
// h applies a function f to e n times, also using n in the function. for example when n = 4 we get f(1,f(2,f(3,f(4,e))))


// A has the type seq<int>
// B and C have the types seq<int*int>
// value of X is seq[0;1;2;3]
// value of Y is seq[(0,0);(1,0);(1,1);(2,0);(2,1);(2,2)]
// value of Z is seq[(0,0);(1,1);(0,1);(2,0);(1,1);(0,2);(3,0);(2,1);(1,2);(0,3)]
// X is just the sequence from 0 to infinity
// Y is a sequence of tuples, where the first element i is only increase once the 2nd element j has gone from 0 i, and then j is set to 0 again
// Z uses the same concept as Y, but the first element is i-j instead of i, and the 2nd element is just j




type Title = string;;
type Setion = Title * Elem list
and Elem = Par of string | Sub of Setion;;
type Chapter = Title * Setion list;;
type Book = Chapter list;;



let maxL xs = List.max xs

let maxLTC xs =
    let rec maxLTC' xs c =
        match xs with
        | [] -> c 0
        | x::tail -> maxLTC' tail (fun res -> c(max res x))
    maxLTC' xs id


type Title = string;;
type Section = Title * Elem list
and Elem = 
    | Par of string 
    | Sub of Section;;
type Chapter = Title * Section list;;
type Book = Chapter list;;


let rec overview = function
    | [] -> []
    | (t,s)::xs -> t::overview xs


let rec depthSection ((_,s)) =
    match s with
    | [] -> 1
    | cs::tail -> 1 + List.fold(fun acc e -> max (depthElem e) acc) 0 (cs::tail)
and depthElem (e)  =
    match e with
    | Par(_) -> 0
    | Sub(xs) -> depthSection xs

let depthChapter (c: Title*Section list) =
    let (_,xs) = c
    1 + List.fold (fun acc s -> max (depthSection s) acc) 0 xs
let depthBook b = 
    List.fold ( fun acc c -> max (depthChapter c) acc) 0 b

let se11 = ("Bakground", [Par "bla"; Sub(("Why programming", [Par "Bla."]))]);;
let se12 = ("An example", [Par "bla"; Sub(("Speial features", [Par "Bla."]))]);;
let se21 = ("Fundamental onepts",
[Par "bla"; Sub(("Mathematial bakground", [Par "Bla."]))]);;
let se22 = ("Operational semantis",
[Sub(("Basis", [Par "Bla."])); Sub(("Appliations", [Par "Bla."]))]);;
let se23 = ("Further reading", [Par "bla"]);;
let se31 = ("Overview", [Par "bla"]);;
let se32 = ("A simple example", [Par "bla"]);;
let se33 = ("An advaned example", [Par "bla"]);;
let se41 = ("Status", [Par "bla"]);;
let se42 = ("What's next?", [Par "bla"]);;
let h1 = ("Introdution", [se11;se12]);;
let h2 = ("Basi Issues", [se21;se22;se23]);;
let h3 = ("Advaned Issues", [se31;se32;se33]);;
let h4 = ("Conlusion", [se41;se42]);;
let book1 = [h1; h2; h3; h4];;

depthBook book1;;


type Numbering = int list;;
type Entry = Numbering * Title;;
type Toc = Entry list;;


type Title = string;;
type Section = Title * Elem list
and Elem = 
    | Par of string 
    | Sub of Section;;
type Chapter = Title * Section list;;
type Book = Chapter list;;



let rec tocS s acc n =
    match s with
    | (_,[]) -> []
    | (t, xs) -> (t, acc@[n])::(tocEl xs (acc@[n]) 1)
and tocEl el acc n =
    match el with
    | [] -> []
    | x::xs -> (tocE x acc@[n] 1) :: tocEl xs acc@[n] (n+1)
and tocE e acc n =
    match e with
    | Par(_) -> []
    | Sub(s) -> tocS s acc 1


// mangler sidste delopgave fordi den er fuuuuucker svær


