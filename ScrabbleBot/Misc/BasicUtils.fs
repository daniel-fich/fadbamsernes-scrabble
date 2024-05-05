module internal BasicUtils

    open System
    open Types

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
        let x = fst coordinate 
        let y = snd coordinate 
    
        sprintf "%d %d %d%c%d " (x) (y) (asciiLetterToAlphabetPos move) (move) (getPointsForLetter move)
        

    let computeOffset (x,y) offset dir =
        if dir = Direction.horizontal then
            (x+offset,y)
        else
            (x,y+offset)
    let otherDir dir = if dir = Direction.horizontal then Direction.vertical else Direction.horizontal
        

