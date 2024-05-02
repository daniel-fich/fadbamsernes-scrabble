namespace YourClientName

open System
open System.Reflection
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open Gaddag
open System.IO
open System.Linq
open System.Threading

open ScrabbleUtil.DebugPrint
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


module GenerateMove =
    
    let rec gen pos word rack arc (state: State.state) =
        if Map.containsKey pos state.board then
           0 
        elif List.isEmpty rack then
            List.filter (fun c -> allowed c arc) rack
            |> List.iter (fun c -> goOn pos c word (List.filter (fun c' -> c' <> c)) (step c arc) arc)
            0
        else
            0
    and goOn pos l word rack newArc oldArc =
        // if po
        ()
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
        
    let computeOffset (x, y) pos direction =
        if direction = Direction.horizontal then
            (x+pos,y)
        else
            (x, y+pos)
    let removeOverlappingLettersOnBoardAndValidate word pos (state: state) dir =
        let charsWithPos = word |> List.mapi (fun i c -> computeOffset pos i dir,c)
                // |> List.filter (fun (coord,_) -> not (Map.containsKey coord board))
        let reBuildFromBoard = ("", charsWithPos) ||> List.fold (fun acc (coords, c) ->
            let c = 
                if Map.containsKey coords state.board then
                    let c,_ = Map.find coords state.board
                    c
                else
                    c
            acc+(Char.ToString c))
        if Dictionary.lookup reBuildFromBoard state.dict then
            Some(charsWithPos |> List.filter (fun (coord,_) -> not (Map.containsKey coord state.board)))
        else
            None
        
       
    let generateApiMoveFromCoordCharList lst =
        ("", lst) ||> List.fold (fun acc (coord, c) ->
            acc+generateValidMoveForApiFromLetter c coord)
    
    let getMoves letters state isStartMove =
        if isStartMove then
            let startmove, pos = ((findAllWordsFromRack letters state) |> List.sortByDescending List.length)[0], (0,0) // No error handling here
            generateValidMoveForApiFromCharList startmove pos Direction.vertical
        else
            let ret = fbm Direction.horizontal state @ fbm Direction.vertical state
            let overlappingRemovedAndPlaysValidated = ret |> List.sortByDescending (fun (_, s,_) -> s.Length) |> List.map (fun (coord,acc,dir) ->
                removeOverlappingLettersOnBoardAndValidate acc coord state dir) |> List.filter Option.isSome |> List.map Option.get
            
            if List.isEmpty overlappingRemovedAndPlaysValidated then
                ""
            else 
                let ret = generateApiMoveFromCoordCharList (overlappingRemovedAndPlaysValidated |> List.head)
                ret
            
            
           
    let findStartWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | "horizontal" ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x-1, y) dir (c :: acc)
                | "vertical" ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x, y-1) dir (c :: acc)
            else
                acc
                    
        aux (x,y) direction []
        
    let findEndWordDir (x, y)  (board: Map<int*int,char*int>) direction =
        let rec aux (x, y) dir (acc: char list) =
            if Map.containsKey (x, y) board then
                match dir with
                | "horizontal" ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x+1, y) dir (c :: acc)
                | "vertical" ->
                    let c = fstTuple (Map.find (x, y) board)
                    aux (x, y+1) dir (c :: acc)
            else
                acc
                    
        aux (x,y) direction [] |> List.rev
     
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
   
    let validate (x,y) direction (word: char list) (st : State.state) =
        printfn "current word in validate is: %A\n" word
        
        let rec aux (x,y) offset =
            if Map.containsKey (x,y) st.board then
                match direction with
                | "horizontal" ->
                    let has_left = Map.containsKey (x , y - 1) st.board
                    let has_right = Map.containsKey (x , y + 1) st.board
                   
                    printfn "has_left %A : has_right %A\n"  has_left has_right
                    
                    let sw =
                        if Map.containsKey (x, y) st.board then
                            if has_left then
                                printfn "Has left entered\n"
                                let c,_ = Map.find (x, y) st.board
                                // (findStartWordDir(x, y-1) st.board "vertical") @ (c :: findEndWordDir (x, y + 1) st.board "vertical")
                                word @ (c :: findEndWordDir (x, y + 1) st.board "vertical")
                            else if has_right then
                                printfn "Has right entered\n"
                                let c,_ = Map.find (x, y) st.board
                                c :: findEndWordDir (x, y + 1) st.board "vertical"
                            else
                                []
                        else
                            []
                            
                    let newWord = sw |> Array.ofList |> String.Concat
                    printfn "newWord %A\n" newWord
                    
                    let exists = Dictionary.lookup newWord st.dict
                    printfn "exists %A\n" exists
                    
                    if not exists then
                        false
                    else
                        aux (x + 1, y) 0
                | "vertical" ->
                    let has_left = Map.containsKey (x - 1, y) st.board
                    let has_right = Map.containsKey (x + 1, y) st.board
                    
                    let sw =
                        if has_left then
                            let c,_ = Map.find (x, y) st.board
                            //(findStartWordDir(x-1, y) st.board "horizontal") @ (c :: findEndWordDir (x + 1, y) st.board "horizontal")
                            word @ (c :: findEndWordDir (x, y + 1) st.board "vertical")
                        else if has_right then
                            let c,_ = Map.find (x, y) st.board
                            c :: findEndWordDir (x + 1, y) st.board "horizontal"
                        else
                            []
                            
                    let newWord = sw |> Array.ofList |> String.Concat
                    let exists = Dictionary.lookup newWord st.dict
                    
                    if not exists then
                        false
                    else
                        aux (x, y + 1) 0
                else
                    true
                    
        if Map.containsKey (x,y) st.board then
            aux (x,y) 0
        else
            false
             
        
    let validWordsAt (x,y) direction lettersHand (st : State.state) =
        let startWord = findStartWordDir (x,y) st.board direction
        let permutationsFromRack = makePermutations lettersHand
        
        printfn "Using startword %A\n" startWord
        
        (List.fold
            (fun acc permutationHand ->
                let currentWord = (findAllWordsFromWord ( startWord @ permutationHand ) st)
                if currentWord <> [] && (validate (x,y) direction currentWord[0] st)
                then currentWord :: acc
                else acc
            )
            []
            permutationsFromRack, startWord)

    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =
        let rec aux (st : State.state) counter=
            Print.printHand pieces (State.hand st)
            debugPrint (sprintf "This is the hand keys: %A\n" (MultiSet.getKeys st.hand))
             
            let lettersHand = uintArrayToLetters (MultiSet.getKeys st.hand)
            
            let startMove = getMoves lettersHand st (Map.isEmpty st.board) // No error handling here
            
            if String.IsNullOrWhiteSpace startMove then
                send cstream SMPass
            else
                let horOrVer = if Map.isEmpty st.board then Direction.vertical else Direction.horizontal
                printfn "startmove: %A\n" (startMove)
                
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the     last inputs)\n\n"
                
                printfn "%A" horOrVer
                let move = RegEx.parseMove startMove 
                
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            // let input =  System.Console.ReadLine()
            // let move = RegEx.parseMove inpt
            
            // let move =
            //     if Map.count st.board = 0 then
            //         let horOrVer = if Map.isEmpty st.board then Direction.horizontal else Direction.vertical
            //         let regexMove = RegEx.parseMove (generateValidMoveForApiFromCharList startMove pos horOrVer)
                    
            //         printf "REGEX GENERATED MOVE START: %A\n" regexMove
            //         regexMove
            //     else
            //         printfn "This is your hand: %A\n" lettersHand
                    
            //         Console.ReadLine()
                    
            //         printfn("Please enter your x now: ")
            //         let x = Console.ReadLine() |> int
                    
            //         printfn("Please enter your y now: ")
            //         let y = Console.ReadLine() |> int
                    
            //         printfn("Please enter your direction now: ")
            //         let direction = Console.ReadLine()
                    
            //         let actualDirection =
            //             if direction = "vertical" then
            //                 Direction.vertical
            //             else
            //                 Direction.horizontal
                           
            //         let move, startWord = (validWordsAt (x,y) direction lettersHand st)// [0][0][1..]
            //         let knownSize = List.length startWord
                    
            //         let generatedMove = move[0][0][knownSize..]
                    
            //         let regexMove =
            //             match actualDirection with
            //             | Direction.horizontal ->
            //                 let regexMove = RegEx.parseMove (generateValidMoveForApiFromCharList generatedMove (x + 1, y) actualDirection)
                            
            //                 regexMove
            //             | Direction.vertical ->
            //                 let regexMove = RegEx.parseMove (generateValidMoveForApiFromCharList generatedMove (x, y + 1) actualDirection)
                            
            //                 regexMove
                    
            //         printf "REGEX GENERATED MOVE: %A\n" regexMove
                    
            //         regexMove
                
            
            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            // send cstream (SMPlay move)
            // if counter > 2 then
            //     while true do
            //         ()
                    
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
                aux {st with board = updated; hand = newHand } (counter+1)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updated = (st.board, toBoardStateWId ms) ||> List.fold (fun s row -> Map.add (snd row) (trd row) s)
                aux {st with board = updated } (counter+1)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st' (counter+1)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
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

        fun () -> playGame cstream tiles (State.mkState Map.empty dict playerNumber handSet)
        