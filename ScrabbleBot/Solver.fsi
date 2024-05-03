module internal Solver
    open State
    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list 
    
    val fbm : Direction -> state -> ((int*int)*char list*Direction) list
    val find_anchors: (int*int) -> (Map<int*int, char*int>) -> (int*int) list
