// 4.18
let rec f g =
    function
    | [] -> []
    | x :: xs -> g x :: f (fun y -> g (g y)) xs

// applies g to the element, and the uses g on the next element twice
// you double the amount of times you apply g after every recursion
// [g x1, g(g x2), g(g(g(g x3))), ...]
