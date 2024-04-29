module internal Solver

    open System
    open MultiSet
    open ScrabbleUtil.Dictionary
    open State

    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list   
    
    
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
    let posToCoord (x, y) pos direction =
        if direction = Direction.horizontal then
            (x+pos, y)
        else
            (x,y+pos)
    let rec gen (x,y) (pos: int) (word: char list) (state:state) (acc: string list) direction =
        if Map.containsKey (x,y) state.board then
            let letter,_ = Map.find (x,y) state.board
            // let isTerminal, nextDict = step letter state.dict |> Option.get
            goon pos (x,y) letter word state.hand (step letter state.dict) acc state direction
        else if not (isEmpty state.hand) then
            ([], getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextHand = removeSingle (asciiLetterToAlphabetPos letter) state.hand
                // let isTerminal, nextDict = step letter state.dict |> Option.get
                goon pos (x,y) letter word nextHand (step letter state.dict) acc state direction |> (@) acc'
            )
        else
            acc
    and goon pos (x,y) letter word rack newArc acc state direction =
        let x',y' = posToCoord (x,y) pos direction
        let leftTaken = Map.containsKey (x',y') state.board
        let rightTaken = Map.containsKey (x',y') state.board
        if pos <= 0 then
            let newWord = letter :: word
            let lonoldarc = step letter state.dict
            let newAcc = conditionalAppend (newWord |> Array.ofList |> String.Concat) acc (Option.isSome lonoldarc)
            if Option.isSome newArc then
                let newDict = Option.get newArc |> snd
                let fstStmnt =
                    if not leftTaken then
                        gen (x, y) (pos-1) newWord {state with dict = newDict; hand = rack } newAcc direction @ newAcc
                    else
                        newAcc
                let newNewDict = reverse newDict
                let sndStmnt =
                    if Option.isSome newNewDict && not leftTaken && not rightTaken then
                      let newNewNewDict = Option.get newNewDict |> snd
                      gen (x,y) 1 word {state with dict = newNewNewDict; hand = rack } newAcc direction @ newAcc
                    else
                        newAcc
                fstStmnt @ sndStmnt
            else
                newAcc
        else
            let newWord = word @ [letter]
            let newAcc = conditionalAppend (newWord |> Array.ofList |> String.Concat) acc (lookup (Char.ToString letter) state.dict && not rightTaken)
            if Option.isSome newArc && not rightTaken then
                let newDict = Option.get newArc |> snd
                gen (x, y) (pos+1) word {state with dict = newDict} newAcc direction
            else newAcc
            
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
        let arc = state.dict
        let ret = gen (x,y) 0 [] state [] dir
        ("", ret) ||> List.fold (fun acc item -> if String.length item > String.length acc then item else acc)
            
    let fbm (dir: Direction) (state: state) =
        let b = state.board
        let k = b.Keys
        k |> Seq.map (fun pos ->
            find_anchors pos b
            |> List.map (fun pos ->
                pos,generate_moves pos Direction.horizontal state
        ))
            
            