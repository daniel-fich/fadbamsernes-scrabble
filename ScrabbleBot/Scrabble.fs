namespace BarelyFunctional

open System
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint
open Solver
open Types

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
 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()


module Scrabble =
    open BasicUtils
    open TileExchange
    open NaiveSolver
    
    let playGame cstream (pieces: Map<uint32, tile>) (st : state) =
        let rec aux (st : state) counter passctr =
            
            let lettersToExchange = computeLettersToExchange st.hand
            let hasPassed =
                if st.playerTurn = st.playerNumber && passctr < 4 then 
                    Print.printHand pieces (hand st)
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
                            let move = computeLongestWord lettersHand st
                            let regexMove =
                                if String.IsNullOrWhiteSpace move then
                                    let move = getMoves st []
                                    RegEx.parseMove move
                                else
                                    RegEx.parseMove move
                            // debugPrint (sprintf "REGEX GENERATED MOVE: %A\n" regexMove)
                            
                            regexMove
                        
                    if List.isEmpty move then
                        if List.isEmpty lettersToExchange then
                            send cstream SMPass
                            true
                        else
                            send cstream (SMChange lettersToExchange)
                            false
                    else
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (Types.playerNumber st) move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)
                        false
                else if passctr > 3 then
                    send cstream SMForfeit
                    false
                else false
            let passincrement = if hasPassed then passctr + 1 else passctr
            let updatedTurn = ((st.playerTurn)%(uint32 (List.length st.playerList)))+1u
            // let updatedTurn = List.item (int updatedTurn) st.playerList
            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            let fst (a, _, _) = a
            let snd (_, b, _) = b
            let trd (_, _, c) = c
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
                aux {st with board = updated; hand = newHand; playerTurn = updatedTurn } (counter+1) 0
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updated = (st.board, toBoardStateWId ms) ||> List.fold (fun s row -> Map.add (snd row) (trd row) s)
                aux {st with board = updated; playerTurn = updatedTurn } (counter+1) 0
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                // let st' = st // This state needs to be updated
                aux {st with playerTurn = updatedTurn } (counter+1) 0
            | RCM (CMGameOver _) -> ()
            | RCM (CMPassed(pid)) ->
                aux {st with playerTurn = updatedTurn } (counter+1) passincrement
            | RCM (CMChangeSuccess(pieces)) ->
                let newHand =
                    lettersToExchange
                    |> MultiSet.ofList
                    |> MultiSet.subtract st.hand
                    |> MultiSet.union (MultiSet.ofListAmount pieces)
                aux {st with playerTurn = updatedTurn; hand = newHand } (counter+1) 0
            | RCM (CMTimeout(pid)) ->
                let newlst = ([],st.playerList) ||> List.fold (fun acc item -> if item <> pid then item::acc else acc)
                aux {st with playerTurn = updatedTurn; playerList = newlst } (counter+1) 0
            | RCM a ->
                aux {st with playerTurn = updatedTurn } (counter+1) 0
                // failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st (counter+1) 0


        aux st 0 0

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

        let playerList = List.init (int numPlayers) (fun idx -> uint32 (idx+1))
        fun () -> playGame cstream tiles (Types.mkState Map.empty dict playerNumber handSet playerTurn numPlayers playerList)
        