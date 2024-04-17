module Gaddag
    type Dict = 
        {
            letters : Map<char,Dict>
            isTerminal : bool
        }
    
    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val step : char -> Dict -> Option<bool*Dict>
    val lookup : string -> Dict -> bool
    val reverse : Dict -> Option<bool*Dict>
