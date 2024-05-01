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
            ' '
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
    let rec gen (x,y) (pos: int) (word: char list) (state:state) (acc: ((int*int)*string) list) direction =
        let offset = posToCoord (x,y) pos direction
        if Map.containsKey offset state.board then
            let letter,_ = Map.find offset state.board
            // let isTerminal, nextDict = step letter state.dict |> Option.get
            goon pos (x,y) letter word state.hand (step letter state.dict) acc state direction
        else if not (isEmpty state.hand) then
            ([], getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextDict = (step letter state.dict)
                if nextDict |> Option.isSome then
                    let nextHand = removeSingle (asciiLetterToAlphabetPos letter) state.hand
                    // let isTerminal, nextDict = step letter state.dict |> Option.get
                    goon pos (x,y) letter word nextHand nextDict acc state direction
                else
                    acc'
            )
        else
            acc
    and goon pos (x,y) letter word rack newArc acc state direction =
        let x',y' = posToCoord (x,y) pos direction
        let leftTaken = Map.containsKey (posToCoord (x,y) (pos-1) direction) state.board
        let rightTaken = Map.containsKey (posToCoord (x,y) (pos+1) direction) state.board
        if pos <= 0 then
            let newWord = letter :: word
            let stepLetter = step letter state.dict
            let isEndOfWord = if Option.isSome stepLetter then stepLetter |> Option.get |> fst else false
            let newAcc = conditionalAppend ((x',y'), newWord
                                                    |> Array.ofList
                                                    |> String.Concat) acc (
                                                        Option.isSome stepLetter
                                                        && not leftTaken
                                                        && not rightTaken)
            if Option.isSome newArc then
                let newDict = Option.get newArc |> snd
                let fstStmnt =
                    if not leftTaken then
                        (gen (x, y) (pos-1) newWord {state with dict = newDict; hand = rack } newAcc direction) 
                    else
                        newAcc
                let newNewDict = reverse newDict
                let sndStmnt =
                    if Option.isSome newNewDict && not leftTaken && not rightTaken then
                      let newNewNewDict = Option.get newNewDict |> snd
                      (gen (x,y) 1 word {state with dict = newNewNewDict; hand = rack } newAcc direction)
                    else
                        newAcc
                fstStmnt @ sndStmnt
            else
                newAcc
        else
            let newWord = word @ [letter]
            let newAcc = conditionalAppend ((x',y'),newWord |> Array.ofList |> String.Concat) acc (lookup (Char.ToString letter) state.dict && not rightTaken)
            if Option.isSome newArc && not rightTaken then
                let newDict = Option.get newArc |> snd
                printfn "Oooooga: %A with pos: %A %A" newWord pos (x,y)
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
        let ret = gen (x,y) 0 [] state [] dir
        // (((0,0),""), ret) ||> List.fold (fun acc item -> if String.length (snd item) > String.length (snd acc) then item else acc)
        ret
            
    let fbm (dir: Direction) (state: state) =
        let b = state.board
        let k = b.Keys
        // let res = k |> Seq.map (fun pos ->
        //     let a = find_anchors pos b
        //     a |> List.map (fun pos ->
        //     generate_moves pos dir state
        // ))
        //
        // res
            
        let ret = generate_moves (-1,0) Direction.horizontal state
        ret
            