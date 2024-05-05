module internal Types

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
        playerTurn    : uint32
        amountPlayers : uint32
        playerList    : uint32 list
    }

    let mkState b d pn h pt ap pl = {
        // board = b;
        board = b
        dict = d
        playerNumber = pn
        hand = h
        playerTurn = pt
        amountPlayers = ap
        playerList = pl 
    }
    
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list   
    

