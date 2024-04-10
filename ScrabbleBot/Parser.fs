module internal Parser

    open Eval
    open ScrabbleUtil
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Utility
    let charListToStr charList = System.String(List.toArray charList) |> string
    let negateExpression = fun n -> Mul ((N -1), n)
    //
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let pid =
        pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>>
        fun (c, cList) -> c :: cList |> charListToStr

    
    let unop = (>*>.)
    let binop pOp p1 p2 = p1 .>*> pOp .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CtermParse, tcref = createParserForwardedToRef<cExp>()
    let CtomParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]


    let sndLvl op f fail = binop (pchar op) AtomParse ProdParse |>> f <?> fail

    let MulParse = sndLvl '*' Mul "Mul" 
    let DivParse = sndLvl '/' Div "Div"
    let ModParse = sndLvl '%' Mod "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]


    let NegParse = unop (pchar '-') AtomParse |>> negateExpression <?> "Mul"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "String"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    let CharToIntParse = pCharToInt >*>. CtermParse |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse 

    let CVParse = pCharValue >*>. TermParse |>> CV
    let ICParse = pIntToChar >*>. TermParse |>> IntToChar
    let C2UpParse = pToUpper >*>. CtomParse |>> ToUpper
    let C2LoParse = pToLower >*>. CtomParse |>> ToLower
    do tcref := choice [CVParse; ICParse; C2LoParse; C2UpParse; CtomParse]

    let CParParse = parenthesise CtomParse
    let CParse =  between (pchar ''') (pchar ''') anyChar  |>> C
    do cref := choice [CParse; CParParse; CtermParse]


    let curry f = fun (a, b) -> f a b
    let CexpParse = CtermParse

    let BTermParse, bref = createParserForwardedToRef<bExp>()
    let BtomParse, btref = createParserForwardedToRef<bExp>()
    
    let AndParse = binop (pstring "/\\") BtomParse BtomParse        |>> curry (.&&.) 
    let OrParse = binop (pstring "\/") BtomParse BtomParse          |>> curry (.||.)
    let NotParse = unop (pchar '~') BtomParse                       |>> (~~)
    let EqParse = binop (pchar '=') TermParse TermParse             |>> curry (.=.)
    let NeqParse = binop (pstring "<>") TermParse TermParse         |>> curry (.<>.)
    let LTParse = binop (pchar '<') TermParse TermParse             |>> curry (.<.)
    let LTEqParse = binop (pstring "<=") TermParse TermParse        |>> curry (.<=.)
    let GTParse = binop (pchar '>') TermParse TermParse             |>> curry (.>.)
    let GTEqParse = binop (pstring ">=") TermParse TermParse        |>> curry (.>=.)
    let IsDigParse = pIsDigit >*>. CtermParse                       |>> IsDigit
    let IsLetParse = pIsLetter >*>. CtermParse                      |>> IsLetter
    let IsVovParse = pIsVowel >*>. CtermParse                       |>> IsVowel

    let BParParse = parenthesise BTermParse
    let TTParse = pTrue |>> fun _ -> TT
    let FFParse = pFalse |>> fun _ -> FF

    let BexpParse = BTermParse
    do bref := choice [AndParse; OrParse; NotParse; EqParse; NeqParse; LTParse; LTEqParse; GTParse; GTEqParse; IsDigParse; IsLetParse; IsVovParse; BtomParse]
    do btref := choice [FFParse; TTParse; BParParse]
    
    let stmntParse = pstring "not implemented"
    let parseSquareProg _ = failwith "not implemented"
    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"
