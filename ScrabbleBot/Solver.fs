module internal ScrabbleBot.Solver

    open System
    open MultiSet
    open ScrabbleUtil.Dictionary

    type Direction =
        | horizontal = 0
        | vertical = 1
    type state = {
        board         : Map<int*int, char*int>
        dict          : Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    } 
    type Rack = char list   
    
    let find_anchors ((x,y): int*int) (board: Map<int*int, char*int>) =
       let viable = [x,y+1;x,y-1;x+1,y;x-1,y]
       ([],viable) ||>
           List.fold (fun acc elm ->
               if Map.containsKey elm board then
                   acc
               else
                   elm :: acc
           )
        
        
    let generate_moves ((x,y): int*int) (dir: Direction) (state: state) =
        ()
            
    let fbm (dir: Direction) (state: state) =
        let b = state.board
        let k = b.Keys
        let res = k |> Seq.map (fun pos ->
            find_anchors pos b
            |> List.iter (fun pos ->
                generate_moves pos Direction.horizontal state
        ))
        0
    let uintToLetter (n: uint32) =
        let baseAscii = int 'A'
        let offset = int n - 1
        if offset >= 0 && offset < 26 then
            char (baseAscii + offset)
        else
            failwith "Input uint must be between 1 and 26"
    let asciiLetterToAlphabetPos (letter: char): uint32 =
        let uppercaseLetter = Char.ToUpper letter
        let asciiA = uint32 'A'
        let asciiLetter = uint32 uppercaseLetter
        if asciiLetter >= asciiA && asciiLetter <= asciiA + 25u then
            asciiLetter - asciiA + 1u
        else
            failwith "Input is not an uppercase letter"

    let conditionalAppend item acc stmt =
        if stmt then
            item::acc
        else acc
    let rec gen ((x,y)) (pos: int) (word: char list) (state:state) (acc: string list) =
        if Map.containsKey (x,y) state.board then
            let letter,_ = Map.find (x,y) state.board
            // let isTerminal, nextDict = step letter state.dict |> Option.get
            goon pos (x,y) letter word state.hand (step letter state.dict) acc state
        else if not (isEmpty state.hand) then
            ([], getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextHand = removeSingle (asciiLetterToAlphabetPos letter) state.hand
                // let isTerminal, nextDict = step letter state.dict |> Option.get
                goon pos (x,y) letter word nextHand (step letter state.dict) acc state |> (@) acc'
            )
        else
            acc
    and goon pos (x,y) letter word rack newArc acc state =
        let roomToLeft = Map.containsKey (x-1,y) state.board
        let roomToRight = Map.containsKey (x+1,y) state.board
        if pos <= 0 then
            let newWord = letter :: word
            let newAcc = conditionalAppend (newWord |> Array.ofList |> String.Concat) acc (lookup (Char.ToString letter) state.dict)
            if Option.isSome newArc then
                let newDict = Option.get newArc |> snd
                let fstStmnt =
                    if roomToLeft then
                        gen (x-1, y) (pos-1) newWord {state with dict = newDict; hand = rack } newAcc
                    else
                        newAcc
                let newNewDict = reverse newDict
                let sndStmnt =
                    if Option.isSome newNewDict && roomToLeft && roomToRight then
                      let newNewNewDict = Option.get newNewDict |> snd
                      gen (x,y) 1 word {state with dict = newNewNewDict; hand = rack } newAcc
                    else
                        newAcc
                fstStmnt @ sndStmnt
            else
                newAcc
        else
            let newWord = word @ [letter]
            let newAcc = conditionalAppend (newWord |> Array.ofList |> String.Concat) acc (lookup (Char.ToString letter) state.dict && roomToRight)
            let newDict = Option.get newArc |> snd
            if Option.isSome newArc && roomToRight then
                gen (x, y) (pos+1) word {state with dict = newDict} newAcc
            else newAcc
            
            
            
            
            