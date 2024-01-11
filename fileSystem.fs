type FileSys = Element list

and Element =
    | File of string * string
    | Dir of string * FileSys

let d1 =
    Dir(
        "d1",
        [ File("a1", "java")
          Dir("d2", [ File("a2", "fsx"); Dir("d3", [ File("a3", "fs") ]) ])
          File("a4", "fsx")
          Dir("d3", [ File("a5", "pdf") ]) ]
    )



let rec namesFileSys =
    function
    | [] -> []
    | e :: es -> (namesElement e) @ (namesFileSys es)
and namesElement =
    function
    | File(s, e) -> [ $"{s}.{e}" ]
    | Dir(s, fs) -> s :: (namesFileSys fs);;


let rec searchFileSys ext filesys =
    match filesys with
    | [] -> Set.empty
    | e :: es -> Set.union (searchElement ext e) (searchFileSys ext es)

and searchElement ext elem =
    match elem with
    | File(s, e) -> if e = ext then (Set.ofList [ s ]) else (Set.empty)
    | Dir(s, fs) -> searchFileSys ext fs



let rec longNamesFileSys =
    function
    | [] -> Set.empty
    | e :: es -> Set.union (longNamesElement e) (longNamesFileSys es)

and longNamesElement =
    function
    | File(s, e) -> Set.ofList [ $"{s}.{e}" ]
    | Dir(s, fs) -> Set.map (fun x -> $"{s}\\{x}") (longNamesFileSys fs)



