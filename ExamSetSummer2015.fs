// problem 1
// 1
let repeat s n =
    let rec repeat' s n c =
        match n with
        | 0 -> c ""
        | _ -> repeat' s (n-1) (fun res -> c(s+res))
    repeat' s n id

// 2
let rec f s1 s2 n =
    match n with
    | 1 -> s1
    | _ -> s1 + "\n" + (f s2 s1 (n-1))


f "ab" "cd" 4;;
f "XO" "OX" 3;;

// 3
let viz r c =
    let rec viz' r c n =
        match c with
        | 1 -> if n % 2 = 0 then repeat "XO" r else repeat "OX" r
        | c' when n%2 = 0 -> (repeat "XO" r) + "\n" + viz' r (c-1) (n+1)
        | c' -> (repeat "OX" r) + "\n" + viz' r (c-1) (n+1)
    viz' r c 0
printfn "%s" (viz 4 5)

// 4
// the function is already tail recursive using continuations
// here it is with accumulating parameter
let repeatAcc s n =
    let rec repeatAcc' s n acc =
        match n with
        | 0 -> acc
        | _ -> repeatAcc' s (n-1) s+acc
    repeatAcc' s n ""

repeatAcc "adraim skal bonge" 10;;




// problem 2
// 1
let rec mixMap f x y =
    match x, y with
    | [],[] -> []
    | [], _ | _, [] -> failwith "Lists are not of equal length"
    |(x::xs, y::ys) -> (f x y) :: mixMap f xs ys

// 2
let rec unmixMap f g = function
    | [] -> []
    | (x,y)::xs -> (f x, g y)::unmixMap f g xs

// 3
// type of mixMap is ('a -> 'b) -> 'a list -> 'a list -> 'b list
// type of unmixMap is ('a -> 'b) -> ('c -> 'd) -> ('a * 'c) list -> ('b * 'd) list

// problem 3
type Tree<'a> =
    | Lf
    | Br of Tree<'a> * 'a * Tree<'a>
        
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;
// 1
let rec reflect t =
    match t with
    | Lf -> Lf
    | Br(t1,x,t2) -> Br(reflect t2, x, reflect t1)

// 2
let accumulate t =
    let rec accumulate' t n =
        match t with
        | Lf -> (Lf,n)
        | Br(t1,n',t2) ->   let newAcc = n+n'
                            let (newLeft, leftAcc) = accumulate' t1 newAcc
                            let (newRight, rightAcc) = accumulate' t2 leftAcc
                            (Br(newLeft, newAcc, newRight), rightAcc)
    let (t', _) = accumulate' t 0
    t';;

    

let rec k i t =
    match t with
    | Lf -> Lf
    | Br(tl,a,tr) -> Br(k (i*i) tl, i*a, k (i*i) tr);;
let rec h n m = function
    | Br(tl,a,tr) when n=m -> h n 1 tl @ [a] @ h n 1 tr
    | Br(tl,_,tr) -> h n (m+1) tl @ h n (m+1) tr
    | Lf -> []
let q n t = h n n t;;


// Give the most general types of k and q and describe what each of these two functions
// computes. Your description for each function should focus on what it computes, rather
// than on individual computation steps.
// k takes a tree t and adds (i^(2^k)) to each middle element of the tree
// where k is the current depth of the tree, starting at 0
// the type of it is int -> Tree<int> -> Tree<int>
// q creates a list of elements from the tree, only adding depth 0, and every nth depth to the list
// it is ordered in the same way as the tree, as in it is ordered correctly from left to right
// type of it is int -> Tree<'a> -> 'a list


// problem 4

type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

let isValidCourseDesc desc = 
    let (_,ects) = desc
    (ects % 5 = 0);;

let isValidCourseBase cb =
    Map.forall (fun x y -> isValidCourseDesc y) cb

let isValidCourseBase' cb =
    Map.fold (fun acc x y -> acc && isValidCourseDesc y) true cb

let cb = Map.ofList [("a",("a",5));("b",("b",10))]


type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

let disjoint s1 s2 =
    Set.empty = Set.intersect s1 s2

let s1 = set ["01";"03"]
let s2 = set ["02";"04"]


let sumECTS s cb =
    Set.fold (fun acc x ->  let (_,ects) = Map.find x cb
                            ects + acc) 0 s;;

let isValidCourseGroup cg cb =
    let (mand, opt) = cg

    ((sumECTS mand cb) <= 45) && ((sumECTS opt cb) <= 45) && (disjoint mand opt) && ((sumECTS (Set.union mand opt) cb) >= 45)



type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>


let isValid fm cb =
    let ((bsn1,bsn2), (tc1,tc2), (pps1,pps2), ep) = fm
    let valid = (isValidCourseGroup (bsn1,bsn2) cb) && (isValidCourseGroup (tc1,tc2) cb) && (isValidCourseGroup (pps1,pps1) cb)
    let allbsn = Set.union bsn1 bsn2
    let alltc = Set.union tc1 tc2
    let allpps = Set.union pps1 pps2
    let snd1 = ((Set.intersect allbsn alltc) = Set.empty)
    let snd2 = ((Set.intersect allpps alltc) = Set.empty)
    let snd3 = ((Set.intersect allbsn allpps) = Set.empty)
    let ebsn = Set.forall(fun x -> ep x) allbsn
    let etc = Set.forall(fun x -> ep x) alltc
    let epps = Set.forall(fun x -> ep x) allpps
    valid && snd1 && snd2 && snd3 && ebsn && etc && epps


