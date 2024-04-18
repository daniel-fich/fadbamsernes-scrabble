namespace YourClientName

open System.Reflection
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open Gaddag
open System.IO

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

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        // board         : Parser.board
        board         : Map<int*int, char*int>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h = {
        // board = b;
        board = b
        dict = d
        playerNumber = pn
        hand = h
    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

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
    let fst (a, _, _) = a
    let snd (_, b, _) = b
    let trd (_, _, c) = c
    let toBoardState ms = ms |> List.map (fun (coord, (tid, (c, v))) ->
                    coord, (c, v)
                )
    let toBoardStateWId ms = ms |> List.map (fun (coord, (tid, (c, v))) ->
                        tid, coord, (c, v)
                    )
    let getWordFromHand (hand: char list) (st: State.state): string =
        rotateFold2 hand
        ""
    // % a k j l i t o
    // a 
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
            
            
        
    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // Pseudo algorithm for choosing a word
            // if Map.isEmpty st.board then
            //      greedily get the first completed word
            //      make some formatting function and set input
            // else
            //      get the first character from the board and try to make a word
            //      check that there are no characters at current pos x+-1
            //      if there is a character move to that one and try to make a word
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

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
        