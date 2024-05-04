namespace YourClientName

open System
open System.Net.Security
open System.Reflection
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open Gaddag
open System.IO
open System.Linq
open System.Threading

open ScrabbleUtil.DebugPrint
open Solver
open State

// The RegEx module is only used to parse human input. It is not used for the final product.
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList
// 0 0 20t1 0 1 9i1 0 2 5e1
 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()


module Scrabble =
    open Solver
        
    let fst (a, _, _) = a
    let snd (_, b, _) = b
    let trd (_, _, c) = c
    
    let fstTuple (a, _) = a
    let sndTuple (_, b) = b
    
   
    let toBoardState ms = ms |> List.map (fun (coord, (tid, (c, v))) ->
                    coord, (c, v)
                )
    let toBoardStateWId ms = ms |> List.map (fun (coord, (tid, (c, v))) ->
                        tid, coord, (c, v)
                    )
    let getWordFromHand (hand: char list) (st: State.state): string =
        rotateFold2 hand
        ""
        
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
                
    let uintToLetter (n: uint32) =
        let baseAscii = int 'A'
        let offset = int n - 1
        if offset >= 0 && offset < 26 then
            char (baseAscii + offset)
        else
            ' '
      
    let asciiLetterToAlphabetPos (letter: char) =
        let uppercaseLetter = Char.ToUpper letter
        let asciiA = int 'A'
        let asciiLetter = int uppercaseLetter
        if asciiLetter >= asciiA && asciiLetter <= asciiA + 25 then
            asciiLetter - asciiA + 1
        else
            failwith "Input is not an uppercase letter"

    
    let uintArrayToLetters (arr: uint32 list) =
        List.map uintToLetter arr

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
    
    let extractValueStep st =
        match st with
        | Some (b, i) -> b, i
        | _ -> failwith "Not implemented"
           
    let findAllWordsFromWord (oWord: char list) (st: State.state) : (char list list) =
        let rec aux word state acc ret =
            match word with
            | [] ->
                ret
            | character :: remaining ->
                let imm = Dictionary.step character state
                       
                if Option.isNone imm then
                    ret
                else 
                    let terminate = fstTuple (extractValueStep imm)
                    let wordList =
                        if terminate then
                            (List.take (acc + 1) oWord) :: ret
                        else
                            ret
                    
                    let newState = sndTuple (extractValueStep imm)
                    let reversed = Dictionary.reverse newState
                    
                    match reversed with
                    | Some r -> aux remaining (sndTuple r) (acc + 1) wordList
                    | None -> aux remaining newState (acc + 1) wordList
                    
        aux oWord st.dict 0 []

    
    let findAllWordsFromRack (hand: char list) (st: State.state) =
        let rec aux hand acc =
            match hand with
            | [] -> acc
            | x :: xs -> aux xs (acc @ findAllWordsFromWord x st)
   
        aux (permutations hand) []
       
    let letterPoints =
        [
            'A', 1; 'B', 3; 'C', 3; 'D', 2; 'E', 1; 'F', 4; 'G', 2;
            'H', 4; 'I', 1; 'J', 8; 'K', 5; 'L', 1; 'M', 3; 'N', 1;
            'O', 1; 'P', 3; 'Q', 10; 'R', 1; 'S', 1; 'T', 1; 'U', 1;
            'V', 4; 'W', 4; 'X', 8; 'Y', 4; 'Z', 10
        ]
        |> Map.ofList 
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
            tooManyOf |> List.map fstTuple
        let orderedByOccurrenceAsc =
            let cids = MultiSet.getKeys hand
            cids |> List.sortByDescending (fun cid ->
                let c = uintToLetter cid |> Char.ToUpper
                if c = ' ' then 100.
                else Map.find c letterPercentages)
                |> List.rev
                |> List.filter (fun cid ->
                    tooManyOfOnlyCid |> List.contains cid |> not)
        
        let amountToExchange = tooManyOf |> List.sumBy sndTuple
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
                
    let getPointsForLetter letter =
        match Map.tryFind letter letterPoints with
        | Some points -> points
        | None -> 0
    
    let generateValidMoveForApiFromLetter (move: char) (coordinate: int*int) =
        let x = fstTuple coordinate 
        let y = sndTuple coordinate 
    
        sprintf "%d %d %d%c%d " (x) (y) (asciiLetterToAlphabetPos move) (move) (getPointsForLetter move)
     
    let generateValidMoveForApiFromCharList (move: char list) (coordinate: int*int) (direction: Direction) =
        let rec aux move acc offset =
            match move with
            | [] ->
                acc
            | x :: xs ->
                let newCoord =
                    match direction with
                    | Direction.horizontal -> (fstTuple coordinate + offset, sndTuple coordinate)
                    | Direction.vertical -> (fstTuple coordinate, sndTuple coordinate + offset)
                    | _ -> failwith "Not going to be hit"
                aux xs (acc + (generateValidMoveForApiFromLetter x newCoord)) (offset + 1)
        aux move "" 0
        
          
    let findStartWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | Direction.horizontal ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x-1, y) dir (c :: acc)
                | Direction.vertical ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x, y-1) dir (c :: acc)
            else
                (x,y),acc
                    
        aux (x,y) direction []
        
    let findEndWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | Direction.horizontal ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x+1, y) dir (c :: acc)
                | Direction.vertical ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x, y+1) dir (c :: acc)
            else
                (x,y),acc
                    
        let coords, res = aux (x,y) direction []
        coords, res |> List.rev
    
    
    let computeOffset (x,y) offset dir =
        if dir = Direction.horizontal then
            (x,y+offset)
        else
            (x+offset,y)
    let otherDir dir = if dir = Direction.horizontal then Direction.vertical else Direction.horizontal
    let toCharListWithCoords word pos dir =
        word |> List.mapi (fun i c -> computeOffset pos i (otherDir dir),c)
    let reBuildFromBoard charsWithPos state =
        ("", charsWithPos) ||> List.fold (fun acc (coords, c) ->
        let c = 
            if Map.containsKey coords state.board then
                Map.find coords state.board |> fstTuple
            else
                c
        acc+(Char.ToString c))
    let rec removeOverlappingLettersOnBoardAndValidate word pos (state: state) dir =
        let charsWithPos = toCharListWithCoords word pos dir
        let boardString = reBuildFromBoard charsWithPos state 
        let crossDir = otherDir dir
        let crossDirectionValid = (true, charsWithPos) ||> List.fold (fun acc (coords, c) ->
                let strt,start = findStartWordDir coords state.board crossDir
                let _,endWrd = findEndWordDir coords state.board crossDir
                if start |> (@) endWrd |> List.length > 1 then
                    let charsWithPos = toCharListWithCoords (start @ endWrd) strt crossDir
                    let boardString = reBuildFromBoard charsWithPos state
                    acc && Dictionary.lookup boardString state.dict
                else
                    acc && true) 
        let crossDirectionValid = crossDirectionValid || true
        if Dictionary.lookup boardString state.dict && crossDirectionValid then
            Some(charsWithPos |> List.filter (fun (coord,_) -> not (Map.containsKey coord state.board)))
        else
            None
            
           
    let generateApiMoveFromCoordCharList lst =
        ("", lst) ||> List.fold (fun acc (coord, c) ->
            acc+generateValidMoveForApiFromLetter c coord)
    
    let getMoves state banned =
        let ret = fbm Direction.horizontal state @ fbm Direction.vertical state
        let overlappingRemovedAndPlaysValidated =
            ret
            |> List.filter (fun (coord,word,dir) ->
                banned
                |> List.contains (toCharListWithCoords word coord dir
                                  |> List.filter (fun (coord,_) ->
                                      not (Map.containsKey coord state.board)
                                      )
                                  )
                |> not
                )
            |> List.sortByDescending (fun (_, s,_) -> s.Length)
            |> List.map (fun (coord,acc,dir) -> removeOverlappingLettersOnBoardAndValidate acc coord state dir)
            |> List.filter Option.isSome
            |> List.map Option.get
        
        if List.isEmpty overlappingRemovedAndPlaysValidated then
            ""
        else 
            let ret = generateApiMoveFromCoordCharList (overlappingRemovedAndPlaysValidated |> List.head)
            ret
                 
         
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
   
    let validate pos direction (word: char list) (startWord: char list) (st : State.state) =
        let wordLength = List.length word - 1
        let startWordLength = List.length startWord - 1
        
        let rec aux (x,y) acc bool =
            // printfn "aux with %A\n" bool
            let leftCoord = computeOffset (x,y) -1 direction
            let rightCoord = computeOffset (x,y) 1 direction
            let leftOp = Map.containsKey leftCoord st.board
            let rightOp = Map.containsKey rightCoord st.board
            let afterOp = Map.containsKey(x,y+1) st.board
        
            let newCoord = computeOffset (x,y) 1 (otherDir direction)
            
            let noLettersAround = not leftOp && not rightOp && not afterOp
            match acc with
            | _ when not bool -> false
            | acc when acc = wordLength -> true
            | _ -> aux newCoord (acc+1) noLettersAround
        
        if Map.containsKey pos st.board && wordLength > startWordLength then
            let newCoord = computeOffset pos 1 (otherDir direction)
            aux newCoord 0 true
        else
            false
             
        
    let validWordsAt pos direction lettersHand (st : State.state) =
        let startWord = findStartWordDir pos st.board direction
        let permutationsFromRack = makePermutations lettersHand
        
        // printfn "Using startword %A\n" startWord
        
        ([], permutationsFromRack)
        ||> (List.fold (fun acc permutationHand ->
                let currentWord = (findAllWordsFromWord ( (sndTuple startWord) @ permutationHand ) st) |> List.sortBy List.length
                // printfn "current word is %A\n" currentWord
                let isValid =
                    if (List.length currentWord)-1 >= 0 then
                        validate pos direction currentWord[(List.length currentWord)-1] (sndTuple startWord) st
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
    
    let generateAllMoves lettersHand (st : State.state) =
        let mv = st.board |> Map.fold (fun keys key _ -> key :: keys) []
        
        let rec auxVert moves acc = 
            match moves with
            | [] -> acc
            | x :: xs -> 
                let x' = fstTuple x
                let y' = sndTuple x
                
                if Map.containsKey(x', y'+1) st.board or Map.containsKey(x'+1, y') st.board then
                    auxVert xs acc
                else
                    let validWords = fstTuple (validWordsAt x Direction.vertical lettersHand st)
                    auxVert xs (validWords @ acc)
            
        let rec auxHori moves acc = 
            match moves with
            | [] -> acc
            | x :: xs ->
                let x' = fstTuple x
                let y' = sndTuple x
                
                if Map.containsKey(x', y'+1) st.board or Map.containsKey(x'+1, y') st.board then
                    auxHori xs acc
                else
                    let validWords = fstTuple (validWordsAt x Direction.horizontal lettersHand st)
                    auxHori xs (validWords @ acc)
                
        (auxHori mv []) @ (auxVert mv [])
        
    
    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =
        let rec aux (st : State.state) counter =
            let lettersToExchange = computeLettersToExchange st.hand
            if st.playerTurn = st.playerNumber then 
                Print.printHand pieces (State.hand st)
                debugPrint (sprintf "This is the hand keys: %A\n" (MultiSet.getKeys st.hand))
                 
                let lettersHand = uintArrayToLetters (MultiSet.getKeys st.hand)
                
                // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                
                let move =
                    if Map.isEmpty st.board then
                        let startMove, pos =((findAllWordsFromRack lettersHand st)
                                             |> List.sortByDescending List.length)[0], (0,0) // No error handling here
                        let regexMove = RegEx.parseMove (generateValidMoveForApiFromCharList startMove pos Direction.vertical)
                        // printf "REGEX GENERATED MOVE START: %A\n" regexMove
                        regexMove
                    else
                        
                        let moves = generateAllMoves lettersHand st
                        
                        let longestString,coord,direction = (longestStrings (moves :: []))[0]
                               
                        let moves, startWord = (validWordsAt coord direction lettersHand st)
                        let knownSize = List.length (sndTuple startWord)
                       
                        let other = otherDir direction
                        let offset = computeOffset coord 1 other
                        let m = longestString[knownSize..]
                        let regexMove =
                            if List.isEmpty m then
                                let move = getMoves st []
                                RegEx.parseMove move
                            else
                                RegEx.parseMove (generateValidMoveForApiFromCharList m offset direction)
                        // debugPrint (sprintf "REGEX GENERATED MOVE: %A\n" regexMove)
                        
                        regexMove
                    
                if List.isEmpty move then
                    if List.isEmpty lettersToExchange then
                        send cstream SMPass
                    else
                        send cstream (SMChange lettersToExchange)
                else
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                        
            let updatedTurn = ((st.playerTurn)%st.amountPlayers)+1u
            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let res = toBoardStateWId ms
                let updated = (st.board, res)
                              ||> List.fold (fun s row -> Map.add (snd row) (trd row) s)
                let newHand = res
                               |> List.map fst 
                               |> MultiSet.ofList
                               |> MultiSet.subtract st.hand
                               |> MultiSet.union (MultiSet.ofListAmount newPieces)
                aux {st with board = updated; hand = newHand; playerTurn = updatedTurn } (counter+1)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updated = (st.board, toBoardStateWId ms) ||> List.fold (fun s row -> Map.add (snd row) (trd row) s)
                aux {st with board = updated; playerTurn = updatedTurn } (counter+1)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                // let st' = st // This state needs to be updated
                aux {st with playerTurn = updatedTurn } (counter+1)
            | RCM (CMGameOver _) -> ()
            | RCM (CMChangeSuccess(pieces)) ->
                let newHand =
                    lettersToExchange
                    |> MultiSet.ofList
                    |> MultiSet.subtract st.hand
                    |> MultiSet.union (MultiSet.ofListAmount pieces)
                aux {st with playerTurn = updatedTurn; hand = newHand } (counter+1)
            | RCM a ->
                aux {st with playerTurn = updatedTurn } (counter+1)
                // failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st (counter+1)


        aux st 0

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerNumber}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        // let board = Parser.mkBoard boardP
        // let res = Dictionary.step 'T' dict
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState Map.empty dict playerNumber handSet playerTurn numPlayers)
        