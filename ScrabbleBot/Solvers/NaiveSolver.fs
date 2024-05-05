module internal NaiveSolver

    open ScrabbleUtil
    open Types
    open BasicUtils
    open SolvingUtil

    let rec findSuitable (hand: char list) (acc: char list) (dict : Dictionary.Dict) =
        match hand with
        | [] -> None
        | x :: xs ->
            let res = Dictionary.step x dict
            if Option.isSome res then
                let isTerminal, dict' = Option.get res
                if isTerminal then
                    Some(acc)
                else
                    findSuitable xs (x::acc) dict'
            else
                None
                
  
    (*
        Note the below code has been copied from
        StackOverflow https://stackoverflow.com/questions/1526046/f-permutations
    *)     
    let rec splitList = function
        | [x] -> [x,[]]
        | x::xs -> (x, xs) :: List.map (fun (y,l) -> y,x::l) (splitList xs)

    let rec permutations = function
        | [] -> [[]]
        | l -> 
            splitList l 
            |> List.collect (fun (x,rest) ->
                 (* permute remaining elements, then prepend head *)
                 permutations rest |> List.map (fun l -> x::l))
    
    let findAllWordsFromWord (oWord: char list) (st: state) : (char list list) =
        let rec aux word state acc ret =
            match word with
            | [] ->
                ret
            | character :: remaining ->
                let imm = Dictionary.step character state
                if Option.isNone imm then
                    ret
                else 
                    let terminate,newState = Option.get imm
                    let wordList =
                        if terminate then
                            (List.take (acc + 1) oWord) :: ret
                        else
                            ret
                    
                    let reversed = Dictionary.reverse newState
                    // let _, reversed = ((false, newState), reversed) ||> Option.defaultValue
                    if Option.isSome reversed then
                        let _,reversed = Option.get reversed
                        aux remaining reversed (acc + 1) wordList
                        @
                        aux remaining newState (acc + 1) wordList
                    else
                        aux remaining newState (acc + 1) wordList
                    
        aux oWord st.dict 0 []

    
    let findAllWordsFromRack (hand: char list) (st: state) =
        let rec aux hand acc =
            match hand with
            | [] -> acc
            | x :: xs -> aux xs (acc @ findAllWordsFromWord x st)
   
        aux (permutations hand) []
       
    
    let generateValidMoveForApiFromCharList (move: char list) (coordinate: int*int) (direction: Direction) =
        let rec aux move acc offset =
            match move with
            | [] ->
                acc
            | x :: xs ->
                let coordOffset = computeOffset coordinate offset direction
                aux xs (acc + (generateValidMoveForApiFromLetter x coordOffset)) (offset + 1)
        aux move "" 0
      
    let rec makePermutations rack =
        let permutationCount = 2f ** (List.length rack |> float32) |> int
        
        let makePermutation letters funnyNumber =
            let rec aux letters funnyNumber list =
                match letters with
                | [] -> list
                | letter :: letters' ->
                    let newList = if funnyNumber % 2 = 1 then letter :: list else list
                    aux letters' (funnyNumber / 2) newList
            aux (List.rev letters) funnyNumber []
        
        List.init permutationCount (makePermutation rack)
   
    let validate pos direction (word: char list) (startWord: char list) (st : Types.state) offset =
        let wordLength = List.length word - 1
        let startWordLength = List.length startWord - 1
        
        let rec aux pos acc bool =
            let other = otherDir direction

            let leftCoord = computeOffset pos (-1*offset) other
            let rightCoord = computeOffset pos offset other
            let afterOpCoord = computeOffset pos offset direction
             
            let posOp c = Map.containsKey c st.board |> not
            
            let leftOp = posOp leftCoord
            let rightOp = posOp rightCoord
            let afterOp = posOp afterOpCoord 
        
            let noLettersAround = leftOp && rightOp && afterOp
            match acc with
            | _ when not bool -> false
            | acc when acc = wordLength -> true
            | _ -> aux afterOpCoord (acc+1) noLettersAround
        
        if Map.containsKey pos st.board && wordLength > startWordLength then
            let newCoord = computeOffset pos offset direction
            aux newCoord 0 true
        else
            false
             
        
    let validWordsAt pos direction lettersHand (st : Types.state) offset =
        let findStartWordDir = if offset > 0 then findStartWordDir else findEndWordDir
        let startWord = findStartWordDir pos st.board direction
        let permutationsFromRack = makePermutations lettersHand
        
        ([], permutationsFromRack)
        ||> (List.fold (fun acc permutationHand ->
                let pos', startWord = startWord
                let currentWord =
                    if offset > 0 then
                        (findAllWordsFromWord (startWord @ permutationHand) st) |> List.sortBy List.length
                    else
                        (findAllWordsFromWord (permutationHand @ startWord) st) |> List.sortBy List.length
                let isValid =
                    if (List.length currentWord)-1 >= 0 then
                        validate pos direction currentWord[(List.length currentWord)-1] startWord st offset
                    else false
                // let currentWord = if offset > 0 then currentWord else List.map List.rev currentWord
                // let pos = if offset > 0 then pos else computeOffset pos (List.length currentWord |> (-) 1 |> (*) -1) direction
                if currentWord <> [] && isValid
                then (currentWord, computeOffset pos' 1 direction, direction, offset) :: acc
                else acc
            )), startWord


    let longestStrings (tuple: (char list list * (int*int)*Direction*int) list list) =
        let longestStringInList (lst: char list list * (int*int) * Direction * int) =
            let lst, c, dir, offset = lst
            let ret = 
                match lst with
                | [] -> []
                | xs -> xs |> List.maxBy _.Length
            ret, c, dir, offset
    
        let longestStringInInnerList (innerList: (char list list * (int*int) * Direction *int) list)=
            match innerList with
            | [] -> [], (0,0), Direction.horizontal, 0
            | xs ->
                xs
                |> List.map longestStringInList
                |> List.maxBy (fun (lst,_,_,_) ->
                    lst.Length
                )
    
        tuple |> List.map longestStringInInnerList 
    
    let generateAllMoves lettersHand (st : state) =
        let allCoords = st.board |> Map.keys |> List.ofSeq
        let rec aux moves acc dir = 
            match moves with
            | [] -> acc
            | x :: xs -> 
                let positiveOffset = computeOffset x 1 dir
                let negativeOffset = computeOffset x -1 dir
                let contains v = Map.containsKey v st.board
                let notContainsPositive = contains positiveOffset |> not
                let notContainsNegative = contains negativeOffset |> not
                if notContainsPositive || notContainsNegative then
                    let validWords =
                        if notContainsNegative then
                            fst (validWordsAt x dir lettersHand st -1)
                        else []
                        
                    let validWords =
                        if notContainsPositive then fst (validWordsAt x dir lettersHand st 1) @ validWords
                        else validWords
                    
                    aux xs (validWords @ acc) dir
                else
                    aux xs acc dir
                
        (aux allCoords [] Direction.horizontal)
        @
        (aux allCoords [] Direction.vertical)
    
    let computeLongestWord letters state =
        let moves = generateAllMoves letters state
                                    
        let overlappingRemovedAndPlaysValidated =
             (longestStrings (moves :: []))
             |> List.map (fun (word, coord, dir, offset) ->
                    // let _, startWord = (validWordsAt coord dir letters state offset)
                    // let knownSize = List.length (snd startWord)
                    let coord =
                        if offset < 0 then
                            // let offsetOffset = if dir = Direction.horizontal then -2 else -1
                            let posOffset = (List.length word |> (*) -2 |> (+) 2)
                            let tmp = computeOffset coord posOffset dir
                            tmp
                        else coord
                    if offset < 0 then 
                        coord, word[1..]@[word[1]], dir
                    else coord, word, dir
                )
             |> List.sortByDescending (fun (_, s,_) -> s.Length)
             |> List.map (fun (coord,acc,dir) -> removeOverlappingLettersOnBoardAndValidate acc coord state dir)
             |> List.filter Option.isSome
             |> List.map Option.get
                 
               
        
        // let _, startWord = (validWordsAt coord direction letters state offset)
        // let knownSize = List.length (snd startWord)
       
        // let offset = computeOffset coord offset direction
        // let m = longestString[knownSize..]
        
        if List.isEmpty overlappingRemovedAndPlaysValidated then
            ""
        else 
            let ret = generateApiMoveFromCoordCharList (overlappingRemovedAndPlaysValidated |> List.head)
            ret
