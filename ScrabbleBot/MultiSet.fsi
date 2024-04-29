// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty : MultiSet<'a>
    val isEmpty : MultiSet<'a> -> bool
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val ofList : List<'a> -> MultiSet<'a>
    val ofListAmount : List<'a*uint32> -> MultiSet<'a>
    val getKeys : MultiSet<'a>  -> 'a list
    
