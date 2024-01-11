
//2.1
let f n =
    match n with
    | n when n % 5 = 0 -> false
    | n when n % 2 = 0 -> true
    | n when n % 3 = 0 -> true
    | _ -> false


//4.3
let rec evenN n =
    match n with
    | 0 -> []
    | _ -> evenN (n-1) @ [n*2]

//4.8
let split xs =
    match xs with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::xs -> let (a, b) = split xs in (x::a, y::b)


//4.9
let zip(xs,ys) =
    if List.length xs <> List.length ys
        then failwith "Lists must be of equal length"
    else
        let rec zip' xs ys =
            match xs, ys with
            | [], [] -> []
            | x::xs, y::ys -> (x, y)::zip' xs ys
        zip' xs ys


//4.11
let rec count(xs,x) =
    match xs with
    | [] -> 0
    | y::ys when y = x -> 1 + count(ys,x)
    | y::ys when y > x -> 0
    | y::ys -> count(ys,x)

let rec insert(xs,x) =
    match xs with
    | [] -> [x]
    | head::tail when head >= x -> x::head::tail
    | head::tail -> head::insert(tail,x)

let rec intersect(xs,ys) =
    match xs,ys with
    | [], _ -> []
    | _, [] -> []
    | x::xs, y::ys when x = y -> x::intersect(xs,ys)
    | x::xs, y::ys when x < y -> intersect(xs,y::ys)
    | x::xs, y::ys -> intersect(x::xs,ys)


let rec plus(xs,ys) =
    match xs,ys with
    | [], [] -> []
    | x::xs, [] -> x::xs
    | [], y::ys -> y::ys
    | x::xs, y::ys when x<y -> x::plus(xs,y::ys)
    | x::xs, y::ys -> y::plus(x::xs,ys)

let rec minus(xs,ys) =
    match xs,ys with
    | [], [] -> []
    | x::xs, [] -> x::xs
    | [], y::ys -> []
    | x::xs, y::ys when x = y -> minus(xs,ys)
    | x::xs, y::ys when x < y -> x::minus(xs,y::ys)
    | x::xs, y::ys -> minus(x::xs,ys)
minus([1;1;1;2;2],[1;1;2;3])
minus([1;1;2;3],[1;1;1;2;2])


//4.12

let rec sum(p,xs) =
    let p x = x > 0

    match xs with
    | [] -> 0
    | x::xs when p x -> x + sum(p,xs)
    | x::xs -> sum(p,xs)


//4.16
let rec f (x, ys) =
    match (x, ys) with
    | (x, []) -> []
    | (x, y::ys) -> (x, y)::f(x-1,ys)

let rec g xs =
    match xs with
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s

let rec h xs =
    match xs with
    | [] -> []
    | x::xs -> x::h xs @[x]

let rec p q = function
      | []    -> []
      | x::xs -> let ys = p q xs
                 if q x then x::ys else ys@[x];;


p (fun x -> x < 5) [1;2;3;4;5;6];;







// exercises for lecture slot 3

// problem 1.1

let rec numberOf x ys =
    match ys with
    | [] -> 0
    | y::ys when y = x -> 1 + numberOf x ys
    | y::ys -> numberOf x ys

// problem 1.2
let positionsOf x ys =
    let rec positionsOf' x ys i =
        match ys with
        | [] -> []
        | y::ys when y = x -> i::positionsOf' x ys (i+1)
        | y::ys -> positionsOf' x ys (i+1)
    positionsOf' x ys 0

// problem 1.3
let rec filterMap p f xs =
    match xs with
    | [] -> []
    | x::xs when p x -> f x::filterMap p f xs
    | x::xs -> filterMap p f xs


// problem 2

let rec splitAt i xs =
    if i<=0 then ([],xs)
    else match xs with
        | [] -> ([],[])
        | x::tail -> let (xs1,xs2) = splitAt (i-1) tail
                     (x::xs1,xs2)

// splitAt -1 [1;2;3] = ([], [1;2;3])
// splitAt 3 [1;2;3;4;5] = ([1;2;3], [4;5])
// splitAt 4 [1;2;3] = ([1;2;3],[])
// int -> 'a list -> 'a list * 'a list
// We take an integer i as input, and then a list of generic type 'a. Then we return a tuple of 2 type 'a lists.

// splitAt k [x0;x1;...;xn-1] return a tuple of 2 lists (ys,zs) where ys = [x0;x1;...;xk-1] and zs = [xk;xk+1;...;xn-1]

// problem 3
let rec f(xs,rs) = 
    match xs with
    | [] -> rs
    | [x] -> x::rs
    | x1::x2::xs -> x1::f(xs,x2::rs)
let g xs = f(xs,[]);;

// g [1;2;3;4;5] =  f([1;2;3;4;5],[]) =
// match case for x1::x2::xs = so we have 1::f([3;4;5],[2])
// match case for x1::x2::xs = so we have 3::f([5],[4;2]) 
// match case for [x] = so we have 5::[4;2]
// becomes [5;4;2]
// bubbles up again to become 3::[5;4;2]
// bubbles up again to become 1::[3;5;4;2]
// [1;3;5;4;2]

// type for f is 'a list * 'a list -> 'a list
// type for g is 'a list -> 'a list


// g takes a list and reorders it.
// it creates 2 lists, the first list being all the even indexed elements of the original list, and the second being all the odd indexed elements of the lists. The odd indexed list is reversed.
// so for [1;2;3;4;5]  we get [1;3;5] and [4;2].
// it then appends the odd indexed list to the even indexed list, and returns the result.
// so we get the result [1;3;5;4;2]
