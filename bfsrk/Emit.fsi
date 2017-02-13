
namespace BF

    open BF.AST

    module Emit =

        type TFilePath = string
        type TOpt = bool
        type TTapeSize = int
        
        type CompilationSettings = {
            input: TFilePath;
            output: TFilePath;
            opt: TOpt;
            ts: TTapeSize;
        }

        val defaultTapeSize : int
        val defaultOutputFile : TFilePath

        val emitCIL : CompilationSettings -> Program -> unit

