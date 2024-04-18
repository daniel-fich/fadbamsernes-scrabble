module Gaddag
    type Dict = 
        {
            letters : Map<char,Dict>
            isTerminal : bool
        }
    
    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val step : char -> Dict -> Option<bool*Dict>
    val allowed : char -> Dict -> bool 
    val lookup : string -> Dict -> bool
    val reverse : Dict -> Option<bool*Dict>
    val rotateFold : 'a list -> 'a list list -> 'a list list
    val rotateFold2 : 'a list -> 'a list list
