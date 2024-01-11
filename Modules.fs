module Vector =
    type Vector = V of float * float

    let (~-.) (V(x, y)) = V(-x, -y)
    let (+.) (V(x1, y1)) (V(x2, y2)) = V(x1 + x2, y1 + y2)
    let (-.) v1 v2 = v1 +. -.v2
    let ( *. ) a (V(x1, y1)) = V(a * x1, a * y1)
    let (&.) (V(x1, y1)) (V(x2, y2)) = x1 * x2 + y1 * y2
    let norm (V(x, y)) = sqrt (x * x + y * y)
    let make (x, y) = V(x, y)
    let coord (V(x, y)) = (x, y)

open Vector

let a = make (1, 2)

let b = make (3.0, 4.0)

let c = 2.0 *. a -. b

coord c

let d = c &. a

let e = norm b
