///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////   LUGGAGE AND FLIGHTS   ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////

type LuggageID          = string //luggage id
type Flight             = string //flight number
type Airport            = string //airport name
type Route              = (Flight * Airport) list //route of journey
type LuggageCatalogue   = (LuggageID * Route) list //a luggage catalogue tracking the path of a luggage

// luggage catalogue:
let LC1 = 
    [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); 
     ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")]);
     ("JR 523-145", [("HP 934","LHR"); ("AU 421","SYD"); ("NP 827","HND")])];;


(* 1. Declare function 'findRoute: Lid*LuggageCatalogue -> Route' *)
let rec findRoute lid lc =
    match lc with
    | []                            -> failwith "No route found for the given luggage ID"
    | (id, list)::tail when lid=id  -> list
    | (id, list)::tail              -> findRoute lid tail

findRoute "SK 222-142" LC1;;
findRoute "LM 222-142" LC1;;
findRoute "DL 016-914" LC1;;
findRoute "JR 523-145" LC1;;
findRoute "SB 472-264" LC1;;


(* 2. Declare function 'inRoute: Flight -> Route -> bool' *)
let rec inRoute f r =
    match r with
    | [] -> false
    | (x,_)::tail when x=f -> true
    | (x,_)::tail -> inRoute f tail

inRoute "HP 934" [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")];;
inRoute "DL 124" [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")];;
inRoute "HP 934" [("HP 934","LHR"); ("AU 421","SYD"); ("NP 827","HND")];;


(* 3. Declare function 'withFlight f lc' *)
let rec withFlight f lc =
    match lc with
    | []                                -> []
    | (lid, (x,y)::s)::tail when f=x    -> lid::withFlight f tail
    | (lid, (x,y)::s)::tail             -> withFlight f tail

withFlight "DL 124" LC1




type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list

let catalogue = [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);
("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]

let rec findRoute (lid, cat) = 
    match cat with
    | [] -> failwith "No route found for the given luggade ID"
    | (id, route)::tail when lid = id -> route
    | _::tail -> findRoute(lid, tail)

let rec inRoute flight route =
    match route with
    | [] -> false
    | (x,y)::xs when x = flight -> true
    | (x,y)::xs -> inRoute flight xs


let rec withFlight f lc =
    match lc with
    | [] -> []
    | (id, route)::tail when inRoute f route -> id::withFlight f tail
    | (_,_)::tail -> withFlight f tail

type ArrivalCatalogue = (Airport * Lid list) list


// function that takes lid, airport and an arrival catalogue
// adds the lid to the aiport in the airport catalogue if the arrival catalogue doesnt already contain that lid for that airport
// if the aiport doesnt exist in the catalogue, add the airport with the lid to the catalogue

let rec extend2 lid ap (ac: (string*(string list)) list) =
    match ac with
    | [] -> [(ap, [lid])]
    | (airport, y)::xs when List.contains lid (y) && airport = ap -> (airport, y)::xs
    | (airport, y)::xs when airport = ap -> (airport, y @ [lid])::xs
    | (airport, y)::xs -> (airport, y)::extend2 lid ap xs


// takes a lid a route and arrival catalog
// uses extend2 to add the lid to all airports in the route to arrival catalogue
let rec extend (lid,r,ac) =
    match r with
    | [] -> []
    | [(_, ap)] -> extend2 lid ap ac
    | (_, ap)::tail -> extend(lid,tail,extend2 lid ap ac)


let toArrivalCatalogue lc =
    let rec toArrivalCatalogue' lc ac =
        match lc with
        | [] -> ac
        | (lid, route)::list -> toArrivalCatalogue' list (extend(lid, route, ac))
    toArrivalCatalogue' lc []