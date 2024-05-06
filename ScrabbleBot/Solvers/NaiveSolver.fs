module internal NaiveSolver

    open ScrabbleUtil
    open Types
    open BasicUtils
    open SolverTools

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
 
    let permutations =
        let rec inner acc curr elms =
            match elms with
            | [] -> curr::acc
            | x ->
                x
                |> List.mapi (
                    fun i elm ->
                        let next = curr @ [elm]
                        let remaining = List.removeAt i elms
                        inner acc next remaining)
                |> List.reduce (@)
        inner [] []


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
                    let _, newState = ((false, newState), reversed) ||> Option.defaultValue 
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
   
    let validate pos direction (word: char list) (startWord: char list) (st : Types.state) =
        let wordLength = List.length word - 1
        let startWordLength = List.length startWord - 1
        
        let rec aux pos acc bool =
            let other = otherDir direction

            let leftCoord = computeOffset pos -1 other
            let rightCoord = computeOffset pos 1 other
            let afterOpCoord = computeOffset pos 1 direction
             
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
            let newCoord = computeOffset pos 1 direction
            aux newCoord 0 true
        else
            false
             
        
    let validWordsAt pos direction lettersHand (st : Types.state) =
        let startWord = findStartWordDir pos st.board direction
        let permutationsFromRack = makePermutations lettersHand
        
        ([], permutationsFromRack)
        ||> (List.fold (fun acc permutationHand ->
                let currentWord = (findAllWordsFromWord ( (snd startWord) @ permutationHand ) st) |> List.sortBy List.length
                let isValid =
                    if (List.length currentWord)-1 >= 0 then
                        validate pos direction currentWord[(List.length currentWord)-1] (snd startWord) st
                    else false
                if currentWord <> [] && isValid
                then (currentWord, pos, direction) :: acc
                else acc
            )), startWord


    let longestStrings (tuple: (char list list * (int*int)*Direction) list list) =
        let longestStringInList (lst: char list list * (int*int) * Direction) =
            let lst, c, dir = lst
            let ret = 
                match lst with
                | [] -> []
                | xs -> xs |> List.maxBy _.Length
            ret, c, dir
    
        let longestStringInInnerList (innerList: (char list list * (int*int) * Direction) list)=
            match innerList with
            | [] -> [], (0,0), Direction.horizontal
            | xs ->
                xs
                |> List.map longestStringInList
                |> List.maxBy (fun (cLst, _, _) -> cLst.Length)
    
        tuple |> List.map longestStringInInnerList 
    
    let generateAllMoves lettersHand (st : state) =
        let allCoords = st.board |> Map.keys |> List.ofSeq
        let rec aux moves acc dir = 
            match moves with
            | [] -> acc
            | x :: xs -> 
                let offset = computeOffset x 1 dir
                if Map.containsKey offset st.board then
                    aux xs acc dir
                else
                    let validWords = fst (validWordsAt x dir lettersHand st)
                    aux xs (validWords @ acc) dir
                
        (aux allCoords [] Direction.horizontal)
        @
        (aux allCoords [] Direction.vertical)
    
    let computeLongestWord letters state =
        let moves = generateAllMoves letters state
                                    
        let moves = generateAllMoves letters state

        let longestString,coord,direction = (longestStrings (moves :: []))[0]

        let _, startWord = (validWordsAt coord direction letters state)

        let knownSize = List.length (snd startWord)

        let offset = computeOffset coord 1 direction

        let m = longestString[knownSize..] 

        (generateValidMoveForApiFromCharList m offset direction) 
