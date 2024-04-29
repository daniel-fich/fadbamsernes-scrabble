module internal Solver
    open State
    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list 
    
    val fbm : Direction -> state -> string list seq
   