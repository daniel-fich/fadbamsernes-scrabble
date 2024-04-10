module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    // temporary
    //let testMultiSet = R (Map.ofList [(1, 2u); (3, 1u)])
    
    let empty = R (Map.empty)

    let isEmpty (ms : MultiSet<'a>) =
        match ms with
        | R (m) -> (Map.count m) = 0

    
    let size (R (m)) = Map.fold (fun s _ count -> s + count) 0u m
    
    let contains (a : 'a) (ms : MultiSet<'a>) =
        match ms with
        | R (m) -> Map.containsKey a m

    let numItems (a : 'a) (ms : MultiSet<'a>) =
        match ms with
        | R (m) when Map.containsKey a m -> Map.find a m
        | R (_) -> 0u

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
    (*
    let rec ofList2 (list : 'a list) : MultiSet<'a> =
        match list with
        | [] -> empty
        | x :: tail -> addSingle x (ofList tail)
    *)
    
    let toList (ms : MultiSet<'a>) : 'a list =
        let createList count x = List.init (int count) (fun _ -> x)
        let folder = fun list x count -> (createList count x) @ list
        fold folder [] ms |> List.rev
    (*
    let rec toList2 (R m) : 'a list =
        let createList count x = List.init (int count) (fun _ -> x)
        match Map.toList m with
        | [] -> []
        | (x, count) :: tail -> (createList count x) @ (tail |> Map.ofList |> R |> toList2)
    
    let rec toList3 (R m) : 'a list =
        match Map.toList m with
        | [] -> []
        | (x, count) :: tail when count = 1u -> x :: (tail |> Map.ofList |> R |> toList3)
        | (x, count) :: tail -> x :: ((x, count - 1u) :: tail |> Map.ofList |> R |> toList3)
    *)


    let map (f : 'a -> 'b) (msa : MultiSet<'a>) : MultiSet<'b> =
        fold (fun msb x count -> add (f x) count msb) empty msa

    (*
    let rec unionFold (folder : 'b -> 'a -> uint32 -> 'a -> uint32 -> 'b) (state : 'b) (ms1 : MultiSet<'a>) (ms2 : MultiSet<'a>) =
        let pairList1 = match ms1 with R m -> Map.toList m
        let pairList2 = match ms2 with R m -> Map.toList m
        match (pairList1, pairList2) with
        | [], [] -> state
        | [], (x2, count2) :: tail2 -> folder (unionFold folder state empty (tail2 |> Map.ofList |> R)) 0 0u x2 count2
        | (x1, count1) :: tail1, [] -> state
        | (x1, count1) :: tail1, (x2, count2) :: tail2 -> state
    *)

    
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
       
    