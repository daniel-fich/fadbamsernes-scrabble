module internal Solver
    open State
    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list 
    
    val fbm : Direction -> state -> ((int*int)*string) list 
   
    val find_anchors: (int*int) -> (Map<int*int, char*int>) -> (int*int) list
