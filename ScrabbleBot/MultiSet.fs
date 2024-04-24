module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let toString (R(s)) =
        s.ToString()
    let empty = R Map.empty

    let isEmpty (R(s)) : bool =
        s |> Map.isEmpty

    let size (R(s)) : uint32 =
        s |> Map.values |> Seq.sum
    
    let contains (key : 'a) (R(s)) : bool =
        s |> Map.containsKey key
    let numItems (a : 'a) (ms : MultiSet<'a>) =
        match ms with
        | R (m) when Map.containsKey a m -> Map.find a m
        | R (_) -> 0u

    let getKeys (R(s)) =
        let rec aux L acc c =
            if c > 0u then
                aux L (L::acc) (c-1u)
            else
                acc
               
        ([], Map.toList s) ||> List.fold (fun acc item ->
            let a, b = item
            let res = aux a [] b
            acc @ res
        )
        
    let add (a : 'a) (count : uint32) (R m) : MultiSet<'a> =
        let newCount = m |> Map.tryFind a |> Option.defaultValue 0u |> (+) count
        R (Map.add a newCount m)
    

    let addSingle (a : 'a) (ms : MultiSet<'a>) : MultiSet<'a> = add a 1u ms
    
    let remove (a : 'a) (count : uint32) (R m) : MultiSet<'a> =
        let aCount = m |> Map.tryFind a |> Option.defaultValue 0u
        if aCount <= count
        then R (Map.remove a m)
        else R (Map.add a (aCount - count) m)

    let removeSingle (a : 'a) (ms : MultiSet<'a>) : MultiSet<'a> = remove a 1u ms


    let fold (folder : 'b -> 'a -> uint32 -> 'b) (state : 'b) (R m) = Map.fold folder state m
    let foldBack (folder : 'a -> uint32 -> 'b -> 'b) (R m) (state : 'b) = Map.foldBack folder m state
    
    let ofList (list : 'a list) : MultiSet<'a> =
        List.fold (fun ms x -> addSingle x ms) empty list
     
    let ofListAmount (list : ('a*uint32) list) : MultiSet<'a> =
            List.fold (fun ms pair -> add (fst pair) (snd pair) ms) empty list
            
    let toList (ms : MultiSet<'a>) : 'a list =
        let createList count x = List.init (int count) (fun _ -> x)
        let folder = fun list x count -> (createList count x) @ list
        fold folder [] ms |> List.rev
   
    let map (f : 'a -> 'b) (msa : MultiSet<'a>) : MultiSet<'b> =
        fold (fun msb x count -> add (f x) count msb) empty msa

    let union (ms1 : MultiSet<'a>) (ms2 : MultiSet<'a>) : MultiSet<'a> =
        let keySet1 = match ms1 with R m -> m |> Map.keys |> Set.ofSeq
        let keySet2 = match ms2 with R m -> m |> Map.keys |> Set.ofSeq
        Set.union keySet1 keySet2 |> Set.fold (fun ms x ->
            let count1 = numItems x ms1
            let count2 = numItems x ms2
            add x (max count1 count2) ms) empty
    
    let sum (s1 : MultiSet<'a>) (s2 : MultiSet<'a>) : MultiSet<'a> = (toList s1) @ (toList s2) |> ofList
    
    let subtract (ms1 : MultiSet<'a>) (ms2 : MultiSet<'a>) : MultiSet<'a> =
        let keySet1 = match ms1 with R m -> m |> Map.keys |> Set.ofSeq
        let keySet2 = match ms2 with R m -> m |> Map.keys |> Set.ofSeq
        Set.union keySet1 keySet2 |> Set.fold (fun ms x ->
            let count1 = numItems x ms1
            let count2 = numItems x ms2
            if count1 <= count2 then ms
            else add x (count1 - count2) ms) empty
    
    let intersection (ms1 : MultiSet<'a>) (ms2 : MultiSet<'a>) : MultiSet<'a> =
        let keySet1 = match ms1 with R m -> m |> Map.keys |> Set.ofSeq
        let keySet2 = match ms2 with R m -> m |> Map.keys |> Set.ofSeq
        Set.union keySet1 keySet2 |> Set.fold (fun ms x ->
            let count1 = numItems x ms1
            let count2 = numItems x ms2
            let minCount = min count1 count2
            if minCount = 0u then ms
            else add x minCount ms) empty
       
    