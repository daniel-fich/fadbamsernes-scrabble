module internal Solver
    open Types
    
    val getMoves : state -> ((int*int)*char) list list -> string
