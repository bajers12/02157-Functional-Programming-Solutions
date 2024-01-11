// 5.3
let sum (p, xs) =
    List.fold
        (fun acc num ->
            match num with
            | x when p x -> x + acc
            | _ -> acc)
        0.0
        xs

let p x = x > 5.0

let list = [ 10.0; 4.0; 13.0; 5.0 ]

sum (p, list)



// Revised Cash Register using list functions

type ArticleCode = string
type ArticleName = string
type Price = int // pr  where  pr >= 0
type Register = (ArticleCode * (ArticleName * Price)) list

let reg =
    [ ("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)) ]


type NoPieces = int // np  where np >= 0
type Item = NoPieces * ArticleCode
type Purchase = Item list

let pur = [ (3, "a2"); (1, "a1") ]

type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price

let rec findArticle ac =
    function
    | (ac', adesc) :: _ when ac = ac' -> adesc
    | _ :: reg -> findArticle ac reg
    | _ -> failwith (ac + " is an unknown article code")



let rec makeBill reg =
    function
    | [] -> ([], 0)
    | (np, ac) :: pur ->
        let (aname, aprice) = findArticle ac reg
        let tprice = np * aprice
        let (billtl, sumtl) = makeBill reg pur
        ((np, aname, tprice) :: billtl, tprice + sumtl)



let makeBill2 list reg =
    List.foldBack
        (fun item acc ->
            match List.tryFind (fun x -> x = item) reg with
            | Some(x) -> x :: acc
            | None -> acc)
        list
        []


makeBill2 pur reg





type ArticleCode = string
type ArticleName = string
type Price = int
type Register = (ArticleCode * (ArticleName * Price)) list

let reg =
    [ ("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)) ]

type NoPieces = int
type Item = NoPieces * ArticleCode
type Purchase = Item list

let pur = [ (3, "a2"); (1, "a1") ]

type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price

let rec findArticle ac reg =
    match List.tryFind (fun (ac', _) -> ac = ac') reg with
    | Some(_, adesc) -> adesc
    | None -> failwith (ac + " is an unknown article code")

let makeBill reg pur =
    let folder (np, ac) (billtl, sumtl) =
        let (aname, aprice) = findArticle ac reg
        let tprice = np * aprice
        ((np, aname, tprice) :: billtl, tprice + sumtl)

    List.foldBack folder pur ([], 0)











type ArticleCode = string
type ArticleName = string
type Price = int
type Register = (ArticleCode * (ArticleName * Price)) list

let reg =
    [ ("a1", ("cheese", 25)); ("a2", ("herring", 4)); ("a3", ("soft drink", 5)) ]

type NoPieces = int
type Item = NoPieces * ArticleCode
type Purchase = Item list

let pur = [ (3, "a2"); (1, "a1") ]
let pur2 = [ (3, "a4") ]



let findArticle ac reg =
    let art = List.tryFind (fun (ac', _) -> ac = ac') reg

    match art with
    | Some(_, adesc) -> adesc
    | _ -> failwith ("Register does not contain " + ac)


let makeBill reg pur =
    let folder (np, ac) (billtl, sumtl) =
        let (aname, aprice) = findArticle ac reg
        let tprice = np * aprice
        ((np, aname, tprice) :: billtl, tprice + sumtl)

    List.foldBack folder pur ([], 0)
