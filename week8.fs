// Problem may 2022 exam set
type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 =
    [ "Introduction to meta-mathematics"
      "To mock a mockingbird"
      "What is the name of this book" ]


let ls0 =
    [ ("Communication and concurrency", "Bob", 4)
      ("Programming in Haskell", "Paul", 2)
      ("Communicating Sequential processes", "Mary", 7)
      ("Elements of the theory of computation", "Dick", 1) ]

let rec onShelf (b: Book) (s: Shelf) : bool =
    match s with
    | [] -> false
    | x :: xs when x = b -> true
    | x :: xs when b >= x -> onShelf b xs
    | _ -> false

let rec toShelf (b: Book) (s: Shelf) : Shelf =
    match s with
    | [] -> [ b ]
    | x :: xs when b < x -> b :: x :: xs
    | x :: xs -> x :: (toShelf b xs)

let fromShelf (b: Book) (s: Shelf) : Shelf option =
    let rec fromShelf' b s =
        match s with
        | [] -> []
        | x :: xs when b = x -> xs
        | x :: xs -> x :: (fromShelf' b s)

    if onShelf b s then Some(fromShelf' b s) else None


let addLoan (b: Book) (n: Name) (d: Date) (ls: Loan list) : Loan list = ls @ [ (b, n, d) ]

let rec removeLoan (b: Book) (n: Name) (ls: Loan list) : Loan list =
    match ls with
    | [] -> []
    | (book, name, _) :: xs when b = book && n = name -> xs
    | x :: xs -> x :: (removeLoan b n xs)


let rec reminders (d: Date) (ls: Loan list) : (Name * Book) list =
    match ls with
    | [] -> []
    | (book, name, date) :: xs when date < d -> (name, book) :: (reminders d xs)
    | x :: xs -> reminders d xs

let rec toLetters (l: (Name * Book) list) : string list =
    match l with
    | [] -> []
    | (n, b) :: xs -> "Dear " + n + "!\nPlease return \"" + b + "\".\nRegards Robin" :: (toLetters xs)



let toLettersMap (l: (Name * Book) list) : string list =
    List.map (fun (b, n) -> "Dear " + n + "!\nPlease return \"" + b + "\".\nRegards Robin") l


let remindersFoldBack (d: Date) (ls: Loan list) : (Name * Book) list =
    List.foldBack (fun (b,n,d') acc -> if d'<d then (n,b)::acc else acc) ls [];;


// Problem 2 week 7
let f x l = List.map (fun n -> (x, n)) l

let f1 x l =
    List.fold (fun acc n -> acc @ [ (x, n) ]) [] l

let f2 x l =
    List.foldBack (fun n acc -> (x, n) :: acc) l []

f 1 [ 1; 2; 3; 4 ]
