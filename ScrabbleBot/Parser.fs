module internal Parser

    open Eval
    open ScrabbleUtil
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    //open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
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

    (*
    Term ::= Prod + Term
		   | Prod - Term
		   | Prod

	Prod ::= Atom * Prod
		   | Atom / Prod
		   | Atom % Prod
	       | Atom
	       
	Atom ::= -Atom
		   | pointValue Atom
		   | charToInt Char
		   | ( Term )
		   | n
		   | v 
    *)
    
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    
    let ParParse = parenthesise TermParse
    let CParParse = parenthesise CharParse

    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "V"
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    let NegParse = pchar '-' >*>. AtomParse |>> negateExpression <?> "PV"
    let CharToIntParse = pCharToInt >*>. CParParse |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; PVParse; CharToIntParse; ParParse; NParse; VParse]

    let quotate p = pchar '\'' >>. p .>> pchar '\''
    let CParse = satisfy (fun _ -> true) |> quotate |>> C <?> "Char"
    let ToUpperParse = pToUpper >*>. CParParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse = pToLower >*>. CParParse |>> ToLower <?> "ToLower"
    let IntToCharParse = pIntToChar >*>. ParParse |>> IntToChar <?> "IntToChar"
    let CVParse = pCharValue >*>. ParParse |>> CV <?> "CV"
    do cref := choice [CParse; CParParse; ToUpperParse; ToLowerParse; IntToCharParse; CVParse]
    
    (*
    pTerm     ::= pProd pTermRest
    
    pTermRest ::= + pProd pTermRest
    
    
    let rec pTerm = pProd .>*>. pTermRest |>> fun (a1, (f, a2)) -> f (a1, a2)
    and pTermRest =
        let pAdd = (pchar '+') >*>. pProd |>> fun a -> (Add, a)
        let emptyAExp = N 0
        let empty = pstring "" |>> fun _ -> (fst, emptyAExp)
        pAdd <|> empty
    and pProd = pAtom
    //and pProdRest = pint32 |>> N <?> "Int"
    and pAtom =
        let nParse = pint32 |>> N <?> "Int"
        let parParse = parenthesise pTerm
        choice [parParse; nParse]
    *)
    
    let AexpParse = TermParse 

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

