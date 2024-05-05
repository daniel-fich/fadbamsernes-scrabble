module internal Solver
    open Types
    
    val fbm : Direction -> state -> ((int*int)*char list*Direction) list
    val find_anchors: (int*int) -> (Map<int*int, char*int>) -> (int*int) list
    val getMoves : state -> ((int*int)*char) list list -> string
