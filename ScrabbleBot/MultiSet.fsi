// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty : MultiSet<'a>
    val isEmpty : MultiSet<'a> -> bool
    val size : MultiSet<'a> -> uint32
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    val filter  : ('a -> uint32 -> bool) -> MultiSet<'a> -> MultiSet<'a>
    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val ofList : List<'a> -> MultiSet<'a>
    val ofListAmount : List<'a*uint32> -> MultiSet<'a>
    val getKeys : MultiSet<'a>  -> 'a list
    val toTupleList : MultiSet<'a>  -> ('a*uint32) list
    val toList : MultiSet<'a>  -> 'a list
    
