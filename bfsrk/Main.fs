module Main

open BF.Parser
open BF.Emit
open System.IO
open System.Reflection
open FParsec
open Argu


/// AST for command line arguments.
type CLArgument =
    | [<MainCommand; ExactlyOnce>]
      Input of file : TFilePath

    | [<Unique; AltCommandLine("-o")>]
      Output of TFilePath

    | [<Unique>]
      Opt

    | [<Unique; EqualsAssignmentAttribute; AltCommandLine("-ts")>]
      TapeSize of TTapeSize

    | Version
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "specify an input file"
            | Output _ -> "specify an output file"
            | Opt -> "compilation optimization flag"
            | TapeSize _ -> "specify BF machine tape size (greater than zero)" 
            | Version -> "display compiler version"

let asmblynm = System.Reflection.Assembly.GetExecutingAssembly().GetName()
let compilerName = asmblynm.Name.ToString()
let compilerVer = asmblynm.Version.ToString()

/// Parser for command line arguments.
let claParser = ArgumentParser.Create<CLArgument>(programName = compilerName + ".exe")

let readProg (filePath:string) =
    use sr = new StreamReader (filePath)
    sr.ReadToEnd()

/// Gets settings from command line arguments.
let getSettings(s:string []) =
    let args = claParser.Parse(s)
    let n = args.GetResult(<@ TapeSize @>, defaultTapeSize)
    if n <= 0 then
        failwith("Tape size must be greater than zero")
    else
        {
            input = args.GetResult(<@ Input @>);
            output = args.GetResult(<@ Output @>, defaultValue = defaultOutputFile) + ".exe";
            opt = args.TryGetResult(<@ Opt @>).IsSome
            ts = n;
        }

let compile(s:CompilationSettings) =
    let lines = readProg s.input
    let par = runParser lines
    match par with
        Success(prog, _, _) -> emitCIL s prog
        | Failure(err, _, _) -> printf "%s\n" err 

[<EntryPoint>]
let main argv = 
    match argv with
        [| "--version" |] ->
            printf "%s %s\n" compilerName compilerVer
        | _ ->
            try
                let settings = getSettings argv
                compile settings
            with
            | ex -> printf "%s\n" ex.Message
    0
