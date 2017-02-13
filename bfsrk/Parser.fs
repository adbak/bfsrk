namespace BF

    open AST
    open FParsec

    module Parser =

        let skipPred (c:char) = ".,<>+-[]Y".IndexOf(c) < 0
        let pSkip = skipManySatisfy skipPred

        let pPrint = stringReturn "." Print
        let pRead = stringReturn "," Read
        let pLeft = stringReturn "<" Left
        let pRight = stringReturn ">" Right
        let pInc = stringReturn "+" Inc
        let pDec = stringReturn "-" Dec
        let pFork = stringReturn "Y" Fork

        let openPar = pstring "[" .>> pSkip
        let closePar = pstring "]"

        let pStmt, pStmtRef = createParserForwardedToRef()

        let pStmts = many pStmt

        let pWhile = between openPar closePar pStmts |>> While

        let pInstr =
            choice [
                pPrint
                pRead
                pLeft
                pRight
                pInc
                pDec
                pWhile
                pFork
            ]

        do pStmtRef := pInstr .>> pSkip

        let pProg = pSkip >>. pStmts .>> eof <?> "end of input" |>> Prog

        let runParser (s:string) = run pProg s