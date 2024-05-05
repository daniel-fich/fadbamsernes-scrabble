module internal TileExchange

    open System
    open BasicUtils

    let letterPercentages = 
        [
            ('E', 11.1607)
            ('A', 8.4966)
            ('R', 7.5809)
            ('I', 7.5448)
            ('O', 7.1635)
            ('T', 6.9509)
            ('N', 6.6544)
            ('S', 5.7351)
            ('L', 5.4893)
            ('C', 4.5388)
            ('U', 3.6308)
            ('D', 3.3844)
            ('P', 3.1671)
            ('M', 3.0129)
            ('H', 3.0034)
            ('G', 2.4705)
            ('B', 2.0720)
            ('F', 1.8121)
            ('Y', 1.7779)
            ('W', 1.2899)
            ('K', 1.1016)
            ('V', 1.0074)
            ('X', 0.2902)
            ('Z', 0.2722)
            ('J', 0.1965)
            ('Q', 0.1962)
        ] |> Map.ofList
            
    let isVowel (c: char) =
        "aeiouAEIOU" |> String.exists ((=) c)
        
    let isConsonant (c: char) =
        not (isVowel c)
     
    let amountOfVowels hand =
        (0u, hand) ||> MultiSet.fold (fun acc cid amount ->
            let c = uintToLetter cid
            if isVowel c then acc+amount else acc)
    let amountOfConsonants hand =
        MultiSet.size hand - (amountOfVowels hand)
        
    let computeLettersToExchange hand =
        let amountOfVowelsOnHand = amountOfVowels hand
        let amountOfConsonantsOnHand = amountOfConsonants hand
        let overflowOfConsonants = amountOfConsonantsOnHand > 5u
        let overflowOfVowels = amountOfVowelsOnHand > 5u
        let overflowOfVowelsOrConsonants = overflowOfVowels || overflowOfConsonants
            
        let hand =
            if overflowOfVowelsOrConsonants then
                let filterfun = if overflowOfVowels then isVowel else isConsonant
                MultiSet.filter (fun cid _ ->
                    let c = uintToLetter cid
                    filterfun c && c <> ' ') hand
            else
                hand
    
        let tooManyOf =
            ([], hand) ||> MultiSet.fold (fun acc cid amount ->
                    if amount > 2u then (cid,amount-1u)::acc else acc)
            
        let tooManyOfOnlyCid =
            tooManyOf |> List.map fst
        let orderedByOccurrenceAsc =
            let cids = MultiSet.getKeys hand
            cids |> List.sortByDescending (fun cid ->
                let c = uintToLetter cid |> Char.ToUpper
                if c = ' ' then 100.
                else Map.find c letterPercentages)
                |> List.rev
                |> List.filter (fun cid ->
                    tooManyOfOnlyCid |> List.contains cid |> not)
            
        let amountToExchange = tooManyOf |> List.sumBy snd
        if amountToExchange >= 4u then
            (MultiSet.empty, tooManyOf) ||> List.fold (fun acc (cid, amount) -> MultiSet.add cid amount acc) |> MultiSet.toList
        else
            let amountRemaining = 4u - amountToExchange
            let leastLikely = orderedByOccurrenceAsc[..int amountRemaining-1]
            let least =
                    leastLikely
                    |> MultiSet.ofList
                    |> MultiSet.toTupleList
                    |> (@) tooManyOf
            (MultiSet.empty, least) ||> List.fold (fun acc (cid, amount) -> MultiSet.add cid amount acc) |> MultiSet.toList
                

