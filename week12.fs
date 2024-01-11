let nat = Seq.initInfinite (fun i -> i)

let idWithPrint i =
    printfn "%d" i
    i

let natWithPrint = Seq.initInfinite idWithPrint

let natWithPrintCached = Seq.cache natWithPrint

Seq.nth 3 natWithPrintCached

Seq.nth 5 natWithPrintCached

Seq.nth 3 natWithPrintCached

Seq.skip 3 natWithPrintCached

let n1 = Seq.initInfinite (fun i -> i)

let n2 = Seq.ofList [ 1; 2; 3; 4 ]

let n3 = Seq.append n1 n2

n3 = Seq.append n1 n2
n1 = n1


let sf = fun () -> seq [ idWithPrint 1 ]
sf ()
let s1 = Seq.delay sf
Seq.nth 0 s1
