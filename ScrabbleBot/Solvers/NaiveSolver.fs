module internal NaiveSolver

    open ScrabbleUtil
    open Types
    open BasicUtils

 
    let toBoardStateWId ms = ms |> List.map (fun (coord, (tid, (c, v))) ->
                        tid, coord, (c, v)
                    )
        
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
                let newCoord =
                    match direction with
                    | Direction.horizontal -> (fst coordinate + offset, snd coordinate)
                    | Direction.vertical -> (fst coordinate, snd coordinate + offset)
                    | _ -> failwith "Not going to be hit"
                aux xs (acc + (generateValidMoveForApiFromLetter x newCoord)) (offset + 1)
        aux move "" 0
        
          
    let findStartWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | Direction.horizontal ->
                    let c = fst (Map.find (x, y) board)
                    aux (x-1, y) dir (c :: acc)
                | Direction.vertical ->
                    let c = fst (Map.find (x, y) board)
                    aux (x, y-1) dir (c :: acc)
            else
                (x,y),acc
                    
        aux (x,y) direction []
        
    let findEndWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | Direction.horizontal ->
                    let c = fst (Map.find (x, y) board)
                    aux (x+1, y) dir (c :: acc)
                | Direction.vertical ->
                    let c = fst (Map.find (x, y) board)
                    aux (x, y+1) dir (c :: acc)
            else
                (x,y),acc
                    
        let coords, res = aux (x,y) direction []
        coords, res |> List.rev
    
    
   
                 
         
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
        
        let rec aux (x,y) acc bool =
            // printfn "aux with %A\n" bool
            let other = otherDir direction
            let leftCoord = computeOffset (x,y) -1 other
            let rightCoord = computeOffset (x,y) 1 other
            let leftOp = Map.containsKey leftCoord st.board
            let rightOp = Map.containsKey rightCoord st.board
            let afterOp = Map.containsKey(x,y+1) st.board
        
            let newCoord = computeOffset (x,y) 1 direction
            
            let noLettersAround = not leftOp && not rightOp && not afterOp
            match acc with
            | _ when not bool -> false
            | acc when acc = wordLength -> true
            | _ -> aux newCoord (acc+1) noLettersAround
        
        if Map.containsKey pos st.board && wordLength > startWordLength then
            let newCoord = computeOffset pos 1 direction
            aux newCoord 0 true
        else
            false
             
        
    let validWordsAt pos direction lettersHand (st : Types.state) =
        let startWord = findStartWordDir pos st.board direction
        let permutationsFromRack = makePermutations lettersHand
        
        // printfn "Using startword %A\n" startWord
        
        ([], permutationsFromRack)
        ||> (List.fold (fun acc permutationHand ->
                let currentWord = (findAllWordsFromWord ( (snd startWord) @ permutationHand ) st) |> List.sortBy List.length
                // printfn "current word is %A\n" currentWord
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
    
    let generateAllMoves lettersHand (st : Types.state) =
        let mv = st.board |> Map.fold (fun keys key _ -> key :: keys) []
        
        let rec auxVert moves acc = 
            match moves with
            | [] -> acc
            | x :: xs -> 
                let x' = fst x
                let y' = snd x
                
                if Map.containsKey(x', y'+1) st.board || Map.containsKey(x'+1, y') st.board then
                    auxVert xs acc
                else
                    let validWords = fst (validWordsAt x Direction.vertical lettersHand st)
                    auxVert xs (validWords @ acc)
            
        let rec auxHori moves acc = 
            match moves with
            | [] -> acc
            | x :: xs ->
                let x' = fst x
                let y' = snd x
                
                if Map.containsKey(x', y'+1) st.board or Map.containsKey(x'+1, y') st.board then
                    auxHori xs acc
                else
                    let validWords = fst (validWordsAt x Direction.horizontal lettersHand st)
                    auxHori xs (validWords @ acc)
                
        (auxHori mv []) @ (auxVert mv [])