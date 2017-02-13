namespace BF

    open FParsec
    open AST

    module Parser =
        val runParser : string -> ParserResult<Program, unit>
