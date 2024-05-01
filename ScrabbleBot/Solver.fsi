module internal Solver
    open State
    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list 
    
    // val fbm : Direction -> state -> Map<int*int,char> list list
    // val fbm : Direction -> state -> string list list list
    val fbm : Direction -> state -> ((int*int)*char list) list
   