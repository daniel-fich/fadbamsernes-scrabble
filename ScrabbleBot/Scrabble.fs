﻿namespace YourClientName

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
    open System.Threading
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
            failwith "Input uint must be between 1 and 26"
      
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
     
    let generateValidMoveForApiFromCharList (move: char list) (coordinate: int*int) =
        let rec aux move acc offset =
            match move with
            | [] ->
                acc
            | x :: xs ->
                let newCoord = (fstTuple coordinate, sndTuple coordinate + offset)
                aux xs (acc + (generateValidMoveForApiFromLetter x newCoord)) (offset + 1)
        aux move "" 0
        
    let getMoves letters state isStartMove =
        if isStartMove then
            ((findAllWordsFromRack letters state) |> List.sortByDescending List.length)[0], (0,0) // No error handling here
        else
            let ret = fbm Direction.horizontal state
            debugPrint (sprintf "%A\n" (ret))
            let pos, str = ret |> Seq.head |> List.head
            str |> Seq.toList, pos
            
    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            debugPrint (sprintf "This is the hand keys: %A\n" (MultiSet.getKeys st.hand))
             
            let lettersHand = uintArrayToLetters (MultiSet.getKeys st.hand)
            
            let startMove, pos = getMoves lettersHand st (Map.isEmpty st.board) // No error handling here
            
            printfn "startmove: %A\n" (startMove)
            printfn "%A\n" (generateValidMoveForApiFromCharList startMove pos)
            
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            // Pseudo algorithm for choosing a word
            // if Map.isEmpty st.board then
            //      greedily get the first completed word
            //      make some formatting function and set input
            // else
            //      get the first character from the board and try to make a word
            //      check that there are no characters at current pos x+-1
            //      if there is a character move to that one and try to make a word
            
            
            // let input =  System.Console.ReadLine()
            // let move = RegEx.parseMove input
            let move = RegEx.parseMove (generateValidMoveForApiFromCharList startMove pos) 
            
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

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
                aux {st with board = updated; hand = newHand }
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updated = (st.board, toBoardStateWId ms) ||> List.fold (fun s row -> Map.add (snd row) (trd row) s)
                aux {st with board = updated }
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

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
        