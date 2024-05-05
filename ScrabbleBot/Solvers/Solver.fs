module internal Solver

    open System
    open MultiSet
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open Types
    open BasicUtils
    open SolverTools

    let lOnOldArc letter state =
        Option.isSome (step letter state.dict)
    let recordPlay anchor pos dir word acc =
        (computeOffset anchor pos dir, word) :: acc
    (*
        Implementation of scrabble move algorithm from paper https://ericsink.com/downloads/faster-scrabble-gordon.pdf
    *)
    let rec gen anchor pos word state acc dir =
        let offset = computeOffset anchor pos dir
        if Map.containsKey offset state.board then
            let letter,_ = Map.find offset state.board
            goon anchor pos letter word (step letter state.dict) state acc dir
        else if not (isEmpty state.hand) then
            (acc, getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextDict = (step letter state.dict)
                if nextDict |> Option.isSome then
                    let nextHand = removeSingle item state.hand
                    goon anchor pos letter word (step letter state.dict) {state with hand = nextHand} acc' dir
                else
                    acc'
            )
        else
            acc
    and goon anchor pos letter word newArc state acc dir =
        let leftTaken = Map.containsKey (computeOffset anchor (pos-1) dir) state.board
        let rightTaken = Map.containsKey (computeOffset anchor (pos+1) dir) state.board
        let rightFromAnchor = Map.containsKey (computeOffset anchor (1) dir) state.board
        if pos <= 0 then
            let word = letter :: word
            let acc = 
                if lOnOldArc letter state && not leftTaken && not rightFromAnchor then
                    recordPlay anchor pos dir word acc
                else
                    acc
            if Option.isSome newArc then
                let newArc = Option.get newArc |> snd
                let acc =
                    if not leftTaken then
                        gen anchor (pos-1) word {state with dict = newArc} acc dir
                    else
                        acc
                let newArc = reverse newArc
                if Option.isSome newArc && not leftTaken && not rightTaken then
                    let newArc = Option.get newArc |> snd
                    gen anchor 1 word {state with dict = newArc} acc dir
                else
                    acc
            else
                acc
        else
            let word = word @ [letter]
            let acc =
                if lOnOldArc letter state && not rightTaken then
                    recordPlay anchor (pos - word.Length + 1) dir word acc
                else
                    acc
            if Option.isSome newArc && not rightTaken then
                let newArc = Option.get newArc |> snd
                gen anchor (pos+1) word {state with dict = newArc} acc dir
            else
                acc
            
    let find_anchors ((x,y): int*int) (board: Map<int*int, char*int>) =
           let viable = [x,y+1;x,y-1;x+1,y;x-1,y]
           ([],viable) ||>
               List.fold (fun acc elm ->
                   if Map.containsKey elm board then
                       acc
                   else
                       elm :: acc
               )
               
    let generate_moves (anchor: int*int) (dir: Direction) (state: state) =
        let ret = gen anchor 0 [] state [] dir
        (((0,0),[]), ret) ||> List.fold (fun acc (coords, word) ->
            if lookup (String.Concat word) state.dict then
                coords, word
            else acc)
            
    let fbm (dir: Direction) (state: state) =
        let b = state.board
        let k = b.Keys
        let res = ([], Seq.toList k) ||> List.fold (fun acc pos ->
            let a = find_anchors pos b
            let moves =
                ([], a) ||> List.fold (fun acc pos ->
                    let coord, res = generate_moves pos dir state
                    if List.length res > 0 then
                        (coord,res,dir) :: acc
                    else
                        acc)
            acc @ moves
            )
        
        res |> List.distinct
    
    
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
