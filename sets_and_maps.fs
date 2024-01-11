let s = Set.ofList [ 1; 2; 3; 4 ]

let s1 = set [ 1; 2; 3; 4 ]

let s2 = set [ "a" ]

let s3 = set [ "c"; "c"; "c" ]

let s4 = set [ "a"; "c"; "b" ]
let s5 = Set.add "d" s4



let males = set [ "Ben"; "Bill"; "Bob" ]

let boardMembers = Set.ofList [ "Alice"; "Bill"; "Ann" ]
let femaleBoard = Set.difference boardMembers males
let nonBoardMales = Set.difference males boardMembers




let reg1 =
    Map.ofList [ ("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)) ]


Map.add "a4" ("bread", 6) reg1

let reg2 = reg1

Map.toList reg2




// Cash Register revised using maps

type ArticleCode = string
type ArticleName = string
type NoPieces = int
type Price = int
type Register = Map<ArticleCode, ArticleName * Price>
type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price
type Purchase = (NoPieces * ArticleCode) list



let (reg: Register) =
    Map.ofList[("a1", ("cheese", 25))
               ("a2", ("herring", 4))
               ("a3", ("soft drink", 5))]


let (pur: Purchase) = [ (3, "a2"); (1, "a1") ]


let rec makeBill (p: Purchase) (reg: Register) : Bill =
    match p with
    | [] -> ([], 0)
    | (np, ac) :: pur ->
        let art = Map.tryFind ac reg

        match art with
        | None -> makeBill pur reg
        | Some(aname, aprice) ->
            let tprice = np * aprice
            let (billt1, sumt1) = makeBill pur reg
            ((np, aname, tprice) :: billt1, tprice + sumt1)
